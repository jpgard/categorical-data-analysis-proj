library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)
library(ggridges)
data_dir <- "../data"
img_dir <- "../img"
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
## get average GPA by student
avg_gpa_df <- df %>% group_by(student_id) %>% summarize(avg_grade = mean(as.numeric(as.character(grade_value)))) %>% ungroup()
df %<>% left_join(avg_gpa_df)
model_df = dplyr::select(df, c("grade_value", "passing_grade", "course_subject.1", "gender", "AT.Director", "AT.Home", "AT.No.Reason", "AT.Other.Reason", "AT.Sick", "institution_id", "total_abs", "student_id", "avg_grade")) %>% na.omit()
########################################################################
######## EXPLORATORY PLOTS
########################################################################
png(file.path(img_dir, "absence_distribution.png"), width = 800, height = 350)
    ggplot(model_df, aes(x = total_abs, fill = grade_value)) + geom_density() + facet_grid(grade_value ~ institution_id) + xlim(c(0, 50)) + scale_fill_grey() + xlab("Count of Absences") + ylab("Density")
dev.off()
png(file.path(img_dir, "grade_gender_distribution.png"), width = 800, height = 350)
    ggplot(model_df, aes(x = grade_value, fill = gender)) + geom_bar() + facet_grid(gender ~ institution_id) + scale_fill_discrete(labels = c("Male", "Female")) + xlab("Grade Value") + ylab("Count") 
dev.off()
# set.seed(20565)
nsamp = 60
stepsz = floor(327/nsamp)
# sample_sids = sample(unique(model_df$student_id), 100)
sample_sids = arrange(avg_gpa_df, desc(avg_grade))[seq(1, nrow(avg_gpa_df), by = stepsz),] %>% pull("student_id")
pdf(file.path(img_dir, "ggjoy.pdf"), width = 8, height = 6.75)
model_df %>% dplyr::filter(student_id %in% sample_sids) %>% 
    ggplot(aes(x = grade_value, y = reorder(student_id, avg_grade), group = student_id, height = ..density..)) + 
    geom_density_ridges(stat = "density", fill = "dodgerblue", alpha = 0.7) + 
    ggtitle("Grade Distributions By Student") + 
    ylab(NULL) + 
    xlab("Grade Value") +
    theme(axis.text.y = element_blank(), axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5), panel.background = element_blank())
dev.off()
pdf(file.path(img_dir, "ggjoy_peaked.pdf"), width = 8, height = 6.75)
model_df %>% dplyr::filter(student_id %in% sample_sids) %>% 
    ggplot(aes(x = grade_value, y = reorder(student_id, avg_grade), group = student_id, height = ..density..)) + 
    geom_ridgeline(stat = "density", fill = "dodgerblue", alpha = 0.7) + 
    ggtitle("Grade Distributions By Student (Sample Shown)") + 
    ylab(NULL) + 
    xlab("Grade Value") + 
    theme(axis.text.y = element_blank(), axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5), panel.background = element_blank())
dev.off()

write_output_table <- function(sumcoefmat, outfile, rounded_digits = 3, pvalcol = "Pr(>|t|)"){
    coefs = sumcoefmat[,"Estimate"]
    exp_coefs = exp(coefs)
    pvals = sumcoefmat[,pvalcol]
    pstars = gtools::stars.pval(pvals)
    ses = sumcoefmat[,"Std. Error"]
    df_out = cbind("Coefficient" = round(coefs, rounded_digits), "Exp(Coefficient)" = round(exp_coefs, rounded_digits), "Std. Error" = round(ses, rounded_digits), "p" = round(pvals, rounded_digits), "Significance" = pstars)
    write.csv(df_out, outfile)
}

########################################################################
######## ADDITIONAL DATA FILTERING
########################################################################

## filter to include only subjects Taylor used
filter_subjects = c("Biology", "Chemistry", "English", "Estonian Language", "History", "Literature", "Mathematics", "Physical Education", "Physics", "Russian Language")
model_df %<>% filter(course_subject.1 %in% filter_subjects)
model_df$course_subject.1 <- droplevels(model_df$course_subject.1)
########################################################################
######## LOGISTIC REGRESSION PASS/FAIL
########################################################################
simpleglm.fit <- glm(passing_grade ~ course_subject.1 + gender + AT.Director + AT.Home + AT.No.Reason + AT.Other.Reason + AT.Sick + institution_id, data = model_df)
summary(simpleglm.fit)
# confint(simpleglm.fit)
write_output_table(summary(simpleglm.fit)$coefficients, outfile = "logistic_regression_abstype.csv")

## NOTE: these results suggest that all of the covariates are "good" predictors, but it's possible
## that there could be some type of confounding happening. Below, we examine penalized logistic
## regression to see if this approach is able to identify which predictors might be most effective.

totalabs.glm.fit <- glm(passing_grade ~ course_subject.1 + gender + total_abs + institution_id, data = model_df)
summary(totalabs.glm.fit)
write_output_table(summary(totalabs.glm.fit)$coefficients, outfile = "logistic_regression_totalabs.csv")


## comparison of AIC (smaller is better)
totalabs.glm.fit$aic
simpleglm.fit$aic

## NOTE: AICs are nearly equivalent; so, we prefer the simpler model because it is easier to interpret 
## write results to output file
# ci_df <- confint(totalabs.glm.fit)
coef_pval_df <- summary(totalabs.glm.fit)$coefficients[,c(1,4)]
# write.csv(cbind(round(coef_pval_df[,2],4), round(ci_df,4), round(coef_pval_df[,2],4), gtools::stars.pval(coef_pval_df[,2])), file = "logistic_regression_coef_pval_ci.csv", row.names = T)
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
lasso.fit$lambda.min
## NOTE: these results show that penalization really isn't effective here, and that all of the
## predictors are probably useful in the model.
## additional note: results differ slightly each time code is run due to random CV folds;
## I forgot to set the random number seed before putting results in paper and writing up :(.

########################################################################
######## ORDERED PROBIT REGRESSION 
########################################################################
library(MASS)
## note: only uses total abs due to collinearity issue
polr.fit = polr(grade_value ~ course_subject.1 + gender + total_abs + institution_id, data = model_df, Hess = T)
polr.summary <- summary(polr.fit)
# polr.ci <- confint(polr.fit)
ctable <- polr.summary$coefficients
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- data.frame(cbind(ctable, "p value" = p))
names(ctable) <- c("Coefficient", "Std. Error", "t", "p")
ctable[,"Exp(Coefficient)"] <- exp(ctable$Coefficient)
ctable <- round(ctable, 3)
ctable[,"Significance"] <- gtools::stars.pval(ctable$p)
ctable %>% 
    dplyr::select(c("Coefficient", "Exp(Coefficient)", "Std. Error", "p", "Significance")) %>%
    write.csv(file = "ordinal_lr.csv")

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
student.institution.fit = glmer(passing_grade ~ factor(course_subject.1) + gender + total_abs + (1|student_id) + + (1|institution_id), data = model_df, family = binomial)
summary(student.institution.fit)
student_institution_coef <- summary(student.institution.fit)$coefficients
write_output_table(summary(student.institution.fit)$coefficients, "mixed_eff_mod_student_instutition.csv", pvalcol = "Pr(>|z|)")
## note: this does not write out random effects; those currently need to be copied into paper manually.
