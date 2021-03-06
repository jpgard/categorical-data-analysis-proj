# Associations between schools
library(data.table)
library(MASS)
source("Taylor/data_prep.R")

# First get the first term for each school
terms = data_sub[,t := unique(term_id)[1], by = institution_id]
terms = terms[, head(.SD, 1), by = institution_id]
data_term = data_sub[term_id %in% terms$t,,]

# Aggregate for each school and each subject
data_agg = data_term[, head(.SD,1), by=.(course_subject_EN, grade_value,c)]
data_agg = data_agg[, .(institution_id, course_subject_EN, grade_value, c),]
data_agg = data_agg[, ':='(grade_value = as.numeric(grade_value),
                           c = as.numeric(c))]

# Fill in the missing grades
full = expand.grid(unique(data_agg$course_subject_EN), c(1.00, 2.00, 3.00, 4.00, 5.00))
full = data.table(course_subject_EN = full$Var1, grade_value = full$Var2)

full_nofail = expand.grid(unique(data_agg$course_subject_EN), c(3.00, 4.00, 5.00))
full_nofail = data.table(course_subject_EN = full_nofail$Var1, grade_value = full_nofail$Var2)

schools = unique(data$institution_id)
data_fin = data.table()
data_nofail = data.table()
for(s in schools) {
  temp = data_agg[institution_id == s,,]
  temp_fail = merge(full, temp, by = c("course_subject_EN", "grade_value"), all.x=TRUE) 
  temp_nofail = merge(full_nofail, temp, by = c("course_subject_EN", "grade_value"), all.x=TRUE) 
  temp_fail[is.na(institution_id), institution_id := s,]
  temp_nofail[is.na(institution_id), institution_id := s,]
  data_fin = rbind(data_fin,temp_fail)
  data_nofail = rbind(data_nofail, temp_nofail)
}
data_fin[is.na(c), c := 0,]
data_nofail[is.na(c), c:=0,]
data_nofail = data_nofail[grade_value >= 3,,]
setkey(data_fin, institution_id, course_subject_EN)

# Now we have full grade records for all common courses for each school.
# Test for homogeniety

##############################
# Disregard the ordinal nature of the grades
# Just do test for independence
##############################

# CMH test for all subjects
tabs = xtabs(c ~ grade_value + institution_id + course_subject_EN, data=data_fin)
mantelhaen.test(tabs)
# # Do it for non failing grades... still get a p-value of 0.
# tabs = xtabs(c ~ grade_value + institution_id + course_subject_EN, data=data_nofail)
# mantelhaen.test(tabs)


#########################
# Plot these densities by school
#########################
tabs_dt = as.data.frame.table(tabs)
tabs_dt = as.data.table(tabs_dt)
tabs_dt$grade_value = as.numeric(tabs_dt$grade_value)
tabs_dt[, prop := Freq / sum(Freq), by = .(course_subject_EN, institution_id)]

ggplot(tabs_dt, aes(grade_value, prop, fill=institution_id)) + geom_density(alpha=.5, stat="identity") +
  facet_wrap(~course_subject_EN, ncol=2) + ylab("Proportion") + xlab("Grade Value") +
  theme(legend.position = "top")

n = length(courses)
course_ind = data.frame(Subject = rep(NA,n), pval = rep(NA,n), sig = rep(NA,n))
options(warn = -1)
for(i in 1:n) {
  temp = data_fin[course_subject_EN == courses[i],]
  temp2 = dcast(temp, grade_value ~ institution_id, value.var = "c", fun.aggregate = sum)
  print(temp2)
  # Another way of showing the data
  # temp3 = t(as.matrix(temp2, nrow = 5, 
  #                     dimnames = list(c("Biology"),c("1","2","3","4","5"), c("519","523", "524"))))
  test = chisq.test(temp2)
  course_ind$Subject[i] = courses[i]
  course_ind$pval[i] = test$p.value
  if(test$p.value < .05) {
    course_ind$sig[i] = TRUE
  }
  else{
    course_ind$sig[i] = FALSE
  }
}
options(warn = 0)


##############################  
# School modeling- check variability in schools
# End up not using.
##############################
# library(lme4)
# # Run mixed effect model with school as random effect
# expand_df = data.table(institution_id = rep(data_fin$institution_id, floor(data_fin$c)),
#                        grade_value = rep(data_fin$grade_value, floor(data_fin$c)),
#                        course_subject_EN = rep(data_fin$course_subject_EN, floor(data_fin$c)))
# summary(factor(expand_df$grade_value))
# expand_df[, fail := ifelse(grade_value <= 3, 1, 0),]
# 
# school.fit = glmer(fail ~ factor(course_subject_EN) + (1|institution_id), data = expand_df, family =binomial)
# 

# 
expand_df2 = data.table(institution_id = rep(data_fin$institution_id, floor(data_fin$c*.01)),
                       grade_value = rep(data_fin$grade_value, floor(data_fin$c*.01)),
                       course_subject_EN = rep(data_fin$course_subject_EN, floor(data_fin$c*.01)))
summary(factor(expand_df$grade_value))
expand_df2[, pass := ifelse(grade_value <= 3, 1, 0),]

school.fit2 = glmer(pass ~ course_subject_EN + (course_subject_EN|institution_id), data = expand_df, family =binomial)
summary(school.fit2)

###################
# Ordinal regression
####################
m = polr(factor(grade_value, ordered = TRUE) ~ factor(institution_id)*factor(course_subject_EN), weights = c, 
         data = data_fin, Hess=TRUE)

ctable <- coef(summary(m))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
ctable <- cbind(ctable, "p value" = p)