# Associations Between Subjects
library(data.table)
source("Taylor/data_prep.R")

# Read in student level data.
students = fread("data/student_grades_total.csv")
students[, ':='(student_id = as.numeric(student_id),
                institution_id = as.numeric(institution_id),
                term_id = as.numeric(term_id),
                course_id = as.numeric(course_id),
                grade_value = as.numeric(grade_value)),]
# Merge in english subjects
stud = merge(students, subs, by = c("institution_id","term_id","course_id"), all.x=TRUE)
stud[, ':='(course_name = NULL,
                course_subject = NULL),]

### Try with two subjects
temp = students[course_subject_EN %in% c("Biology", "Chemistry"),,]

