# Associations between schools
library(data.table)
source("Taylor/data_prep.R")

# Aggregate for each school and each subject
data_agg = data_sub[, count := sum(as.numeric(c)), by=.(institution_id, course_subject_EN, grade_value)]
data_agg = data_agg[, head(.SD,1), by=.(course_subject_EN, grade_value,count)]
data_agg = data_agg[, .(institution_id, course_subject_EN, grade_value, count),]
data_agg = data_agg[, ':='(grade_value = as.numeric(grade_value),
                           count = as.numeric(count))]

# Fill in the missing grades
full = expand.grid(unique(data_agg$course_subject_EN), c(1.00, 2.00, 3.00, 4.00, 5.00))
full = data.table(course_subject_EN = full$Var1, grade_value = full$Var2)

schools = unique(data$institution_id)
data_fin = data.table()
for(s in schools) {
  temp = data_agg[institution_id == s,,]
  temp = merge(full, temp, by = c("course_subject_EN", "grade_value"), all.x=TRUE) 
  temp[is.na(institution_id), institution_id := s,]
  data_fin = rbind(data_fin,temp)
}
data_fin[is.na(count), count := 0,]
setkey(data_fin, institution_id, course_subject_EN)

# Now we have full grade records for all common courses for each school.
# Test for homogeniety

# For each subject
temp = data_fin[course_subject_EN == "Biology",]
temp2 = dcast(temp, grade_value ~ institution_id, value.var = "count")
chisq.test(temp2)

ftable(matrix(temp2[]))
temp3 = t(as.matrix(temp2, nrow = 5, 
                    dimnames = list(c("Biology"),c("1","2","3","4","5"), c("519","523", "524"))))

##############################
# Disregard the ordinal nature of the grades
# Just do test for independence
##############################
n = length(courses)
course_ind = data.frame(Subject = rep(NA,n), pval = rep(NA,n), sig = rep(NA,n))
options(warn = -1)
for(i in 1:n) {
  temp = data_fin[course_subject_EN == courses[i] & grade_value > 2,]
  temp2 = dcast(temp, grade_value ~ institution_id, value.var = "count")
  print(temp2)
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

# Seems that most of the 0's are at the 1 and 2 grade level.
# Even taking those out and doing a test for independence between values 3, 4 and 5 
## we get that all the p-values are 0.
## There is no evidence that the schools have different grade distributions.

