# Data Prep
library(data.table)
# Read in the subject information:
subs = fread("data/course_institution_info_english.csv")
subs$course_id = as.numeric(subs$course_id)
subs$term_id = as.numeric(subs$term_id)
subs$institution_id = as.numeric(subs$institution_id)
# Aggregate the names
subs[, course_name := NULL,]
subs_unq = subs[,head(.SD, 1), by = .(institution_id, term_id, course_subject_EN)]
# Read in the grade counts
grades = fread("data/grade_counts.csv")
grades$course_id = as.numeric(grades$course_id)
grades$term_id = as.numeric(grades$term_id)
grades$institution_id = as.numeric(grades$institution_id)
# Merge the two together
data = merge(grades, subs, by = c("institution_id","term_id", "course_id"), all.x=TRUE)

# We are now going to only get subjects that are in all three schools.
schools = unique(data$institution_id)
un_courses = list()
for(s in schools) {
  terms = unique(data[institution_id == s, term_id,])
  for(t in terms) {
    un_courses[[as.character(paste0(s,"-",t))]] = unique(subs[institution_id == s & term_id == t, 
                                                             course_subject_EN,])  
  }
}
courses = Reduce(intersect, un_courses)
