library(dplyr)
data_dir <- "../data"
grade_df <- dplyr::bind_rows(
    read.csv("../data/data-1513093634824_524.csv", stringsAsFactors = F),
    read.csv("../data/data-1513093597468_519.csv", stringsAsFactors = F),
    read.csv("../data/data-1513093558934_523.csv", stringsAsFactors = F))
course_df <- read.csv(file.path(data_dir, "course_institution_info_english.csv"))

temp <- dplyr::inner_join(grade_df, course_df, by = c("course_id", "term_id", "institution_id"))
if (nrow(temp) == nrow(grade_df)){
    grade_df <- temp
} else{
    message("Warning: potential join issue between absence and course data.")
}

########################################################################
######## LOGLINEAR MODELING
########################################################################

grade_counts <- grade_df %>% 
    mutate(gender = factor(gender), 
           grade_value = factor(grade_value, ordered = T), 
           institution_id = factor(institution_id),
           course_subject.1 = factor(course_subject.1)) %>% 
    dplyr::group_by(institution_id, course_subject.1, gender, grade_value) %>% dplyr::count()
n_subjects = 5
top_n_subjects = grade_df %>% group_by(course_subject.1) %>% count() %>% ungroup() %>% top_n(n_subjects, wt = n) %>% pull("course_subject.1")
grade_counts_topn = grade_counts[grade_counts$course_subject.1 %in% top_n_subjects,]
fit.saturated = glm(n ~ grade_value * course_subject.1 *gender * institution_id, grade_counts_topn, family="poisson")
# summary(fit.saturated)
fit.saturated$deviance # note that deviance is almost exactly zero for this model
pchisq(fit.saturated$deviance, fit.saturated$df.residual, lower.tail = F)
## from saturated model, remove individual 2-way interactions with grade_value

# remove grade_value*gender
fit.sat.noGVgender = glm(n ~ grade_value * course_subject.1 *gender * institution_id - grade_value*gender, grade_counts_topn, family="poisson")
pchisq(fit.sat.noGVgender$deviance - fit.saturated$deviance, 
        df = fit.sat.noGVgender$df.residual - fit.saturated$df.residual, 
        lower.tail = F)

fit.sat.noGVinstitution = glm(n ~ grade_value * course_subject.1 *gender * institution_id - grade_value*institution_id, grade_counts_topn, family="poisson")
pchisq(fit.sat.noGVinstitution$deviance - fit.saturated$deviance, 
       df = fit.saturated$df.residual - fit.sat.noGVinstitution$df.residual, 
       lower.tail = F)

fit.sat.noGVsubject = glm(n ~ grade_value * course_subject.1 *gender * institution_id - grade_value*course_subject.1, grade_counts_topn, family="poisson")
pchisq(fit.sat.noGVsubject$deviance - fit.saturated$deviance, 
       df = fit.saturated$df.residual - fit.sat.noGVsubject$df.residual, 
       lower.tail = F)

fit.sat.noGVsubject = glm(n ~ grade_value * course_subject.1 *gender * institution_id - grade_value*course_subject.1, grade_counts_topn, family="poisson")
pchisq(fit.sat.noGVsubject$deviance - fit.saturated$deviance, 
       df = fit.saturated$df.residual - fit.sat.noGVsubject$df.residual, 
       lower.tail = F)

fit.sat.noGV = glm(n ~ grade_value + course_subject.1 *gender * institution_id, grade_counts_topn, family="poisson")
pchisq(fit.sat.noGV$deviance - fit.saturated$deviance, 
       df = fit.sat.noGV$df.residual- fit.saturated$df.residual, 
       lower.tail = F)


# conditional independence of all variables
fit.indep = glm(n ~ grade_value + course_subject.1 + institution_id + gender, grade_counts_topn, family="poisson")
pchisq(fit.indep$deviance, fit.indep$df.residual, lower.tail = F)
## course and institution
fit.course.institution = glm(n ~ grade_value + course_subject.1 * institution_id + gender, grade_counts_topn, family="poisson")

## course and institution and gender
fit.course.institution = glm(n ~ grade_value + course_subject.1 * institution_id + gender, grade_counts_topn, family="poisson")

## difference between conditional independence and saturated model
pchisq(fit.indep$deviance - fit.saturated$deviance, fit.indep$df.residual - fit.saturated$df.residual, lower.tail = F)

fit.loglin = glm(n ~ grade_value + course_subject.1 *gender * institution_id, grade_counts_topn, family="poisson")
summary(fit.loglin)
cbind(data.frame(coef(fit.loglin), data.frame(confint(fit.loglin))))    
fit.loglin2 = glm(n ~ grade_value + course_subject.1 *gender + institution_id, grade_counts_topn, family="poisson")
ddev =  fit.loglin2$deviance- fit.loglin$deviance 
pchisq(ddev, 1, lower.tail = F)
ddev

## TODO: explore dummies for first term of year or last term of year

########################################################################
######## GRADE PREDICTION WITH MULTINOMIAL LR
########################################################################
library(magrittr)
grade_df %<>%
    mutate(gender = factor(gender), 
           grade_value = factor(grade_value, ordered = T), 
           institution_id = factor(institution_id),
           course_subject.1 = factor(course_subject.1))
mnglm.fit <- nnet::multinom(grade_value ~ gender + course_subject.1 + institution_id, data = grade_df)
mnglm.sum = summary(mnglm.fit)
t(mnglm.sum$coefficients)


########################################################################
######## GRADE PREDICTION WITH LASSO
########################################################################
## TODO: binary outcome with 1,2,3 vs 4,5
## create dummy variables
grade_dummy_mx <- model.matrix(grade_value ~ gender + course_subject.1 + institution_id - 1,grade_df)
library(glmnet)
lasso.fit = cv.glmnet(grade_dummy_mx, grade_df$grade_value, family = "multinomial", alpha=1)
plot(lasso.fit)
plot(lasso.fit$glmnet.fit)
coef(lasso.fit)
