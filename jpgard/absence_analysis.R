library(dplyr)
library(tidyr)
data_dir <- "../data"
grade_df <- dplyr::bind_rows(
    read.csv(file.path(data_dir, "data-1513093634824_524.csv"), stringsAsFactors = F),
    read.csv(file.path(data_dir, "data-1513093597468_519.csv"), stringsAsFactors = F),
    read.csv(file.path(data_dir, "data-1513093558934_523.csv"), stringsAsFactors = F))
course_df <- read.csv(file.path(data_dir, "course_institution_info_english.csv"))
grade_df <- dplyr::bind_rows(
    read.csv("../data/data-1513093634824_524.csv", stringsAsFactors = F),
    read.csv("../data/data-1513093597468_519.csv", stringsAsFactors = F),
    read.csv("../data/data-1513093558934_523.csv", stringsAsFactors = F)) %>%
    mutate(grade_value = factor(grade_value, ordered = T))
absence_df <- dplyr::bind_rows(
    read.csv(file.path(data_dir, "data-1513170869240_absence524.csv"), stringsAsFactors = F),
    read.csv(file.path(data_dir, "data-1513170800805_absence519.csv"), stringsAsFactors = F),
    read.csv(file.path(data_dir, "data-1513170753251_absence523.csv"), stringsAsFactors = F)
    ) %>%
    group_by(institution_id, course_id, student_id, absence_type) %>%
    summarise(absence_day_count = sum(absence_length_days)) %>%
    ungroup()
absence_types <- read.csv(file.path(data_dir, "absence_type.csv"), stringsAsFactors = F)
absence_df %<>% left_join(absence_types) 
absence_df %>% 
    group_by(absence_type_group, absence_type_english) %>% 
    summarise(total_days = sum(absence_day_count)) %>% 
    arrange(desc(absence_type_group, total_days))

absence_df %<>% 
    tidyr::spread("absence_type_group", value = "absence_day_count") %>% 
    mutate_all(funs(replace(., is.na(.), 0)))

## NOTE: this join creates multiple records for courses which occur over multiple terms; 
## the correct interpretation is of absences as TOTAL NUMBER OF COURSE ABSENCES for the entire course.
df = absence_df %>% 
    left_join(course_df) %>% 
    left_join(grade_df) %>%
    mutate(gender = factor(gender),
           institution_id = factor(institution_id),
           student_id = factor(student_id))

names(df) <- make.names(names(df))
df$passing_grade <- df$grade_value > 3
## get total absences by student
df %<>% rowwise() %>% mutate(total_abs = sum(AT.Director + AT.Home + AT.No.Reason + AT.Other.Reason + AT.Sick))
########################################################################
######## LOGISTIC REGRESSION PASS/FAIL
########################################################################
model_df = dplyr::select(df, c("grade_value", "passing_grade", "course_subject.1", "gender", "AT.Director", "AT.Home", "AT.No.Reason", "AT.Other.Reason", "AT.Sick", "institution_id", "total_abs", "student_id")) %>% na.omit()
simpleglm.fit <- glm(passing_grade ~ course_subject.1 + gender + AT.Director + AT.Home + AT.No.Reason + AT.Other.Reason + AT.Sick + institution_id, data = model_df)
summary(simpleglm.fit)
confint(simpleglm.fit)
## NOTE: these results suggest that all of the covariates are "good" predictors, but it's possible
## that there could be some type of confounding happening. Below, we examine penalized logistic
## regression to see if this approach is able to identify which predictors might be most effective.

totalabs.glm.fit <- glm(passing_grade ~ course_subject.1 + gender + total_abs + institution_id, data = model_df)
summary(totalabs.glm.fit)

## comparison of AIC (smaller is better)
totalabs.glm.fit$aic
simpleglm.fit$aic
## NOTE: AICs are nearly equivalent; so, we prefer the simpler model because it is easier to interpret 

########################################################################
######## LASSO PASS/FAIL
########################################################################
## create dummy variables
model_mx <- model.matrix(passing_grade ~ course_subject.1 + gender + AT.Director + AT.Home + AT.No.Reason + AT.Other.Reason + AT.Sick + total_abs - 1, model_df)
library(glmnet)
lasso.fit = cv.glmnet(model_mx, model_df$passing_grade, family="binomial", alpha=1)
plot(lasso.fit)
plot(lasso.fit$glmnet.fit)
coef(lasso.fit)
## NOTE: these results show that penalization really isn't effective here, and that all of the
## predictors are probably useful in the model.

########################################################################
######## ORDERED PROBIT REGRESSION 
########################################################################
library(MASS)
## note: only uses total abs due to collinearity issue
polr.fit = polr(grade_value ~ course_subject.1 + gender + total_abs, data = model_df, Hess = T)
summary(polr.fit)

########################################################################
######## RANDOM EFFECTS
########################################################################
library(lme4)
## student-level effects; also look at student and instutition-level effects
## use binary outcome for this; glmer
## comare analysis of deviance for each
## only analyze "best" model
## institution-varying intercepts
school.only.fit = glmer(passing_grade ~ factor(course_subject.1) + gender + total_abs + (1|institution_id), data = model_df, family = binomial)
summary(school.only.fit)
## student-varying intercepts
student.only.fit = glmer(passing_grade ~ gender + total_abs + (1|student_id), data = model_df, family = binomial)
summary(student.only.fit)
## student- and institution-varying intercepts
student.only.fit = glmer(passing_grade ~ factor(course_subject.1) + gender + total_abs + (1|student_id) + + (1|institution_id), data = model_df, family = binomial)
summary(student.only.fit)


