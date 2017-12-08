# Associations Between Subjects
source("Taylor/data_prep.R")
######################################
# Run Tests For Independence
## But this is ordinal data.
######################################
# Aggregate down to just the term and course, forget school.
data_agg = data_sub[, count := sum(as.numeric(c)), by=.(course_subject_EN, grade_value)]
data_agg = data_agg[, head(.SD,1), by=.(course_subject_EN, grade_value,count)]
data_agg = data_agg[, .(course_subject_EN, grade_value, count),]
data_agg = data_agg[, ':='(grade_value = as.numeric(grade_value),
                           count = as.numeric(count))]

# Fill in the missing grades
full = expand.grid(unique(data_agg$course_subject_EN), c(1.00, 2.00, 3.00, 4.00, 5.00))
full = data.table(course_subject_EN = full$Var1, grade_value = full$Var2)

data_agg = merge(full, data_agg, by = c("course_subject_EN", "grade_value"), all.x=TRUE)
data_agg[is.na(count), count := 0,]


# Now we can begin our tests for independence

bio = data_agg[course_subject_EN =="Biology",]
chem = data_agg[course_subject_EN == "Chemisty",]


