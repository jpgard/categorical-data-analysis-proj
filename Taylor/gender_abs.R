# Prepare data to merge absences and gender to grades
library(dplyr)
library(tidyr)
library(magrittr)
library(data.table)

# Read in the absence data
absence_df <- dplyr::bind_rows(
  read.csv("data/data-1513170869240_absence524.csv", stringsAsFactors = F),
  read.csv("data/data-1513170800805_absence519.csv", stringsAsFactors = F),
  read.csv("data/data-1513170753251_absence523.csv", stringsAsFactors = F)
) %>%
  group_by(institution_id, course_id, student_id, absence_type) %>%
  summarise(absence_day_count = sum(absence_length_days)) %>%
  ungroup()
absence_types <- read.csv(file.path(data_dir, "absence_type.csv"), stringsAsFactors = F)
absence_df %<>% left_join(absence_types)
absence_df %>% group_by(absence_type_group, absence_type_english) %>% summarise(total_days = sum(absence_day_count)) %>% arrange(desc(absence_type_group, total_days))

absence_df %<>% 
  tidyr::spread("absence_type_group", value = "absence_day_count") %>% 
  mutate_all(funs(replace(., is.na(.), 0)))

absence_dt = as.data.table(absence_df)
absence_dt[,absence_type := NULL,]
absence_dt[,absence_type_english := NULL,]
absence_dt[, ':='(`AT Director` = sum(`AT Director`),
                  `AT Home` = sum(`AT Home`),
                  `AT No Reason` = sum(`AT No Reason`),
                  `AT Other Reason` = sum(`AT Other Reason`),
                  `AT Sick` = sum(`AT Sick`)), by=.(institution_id, course_id, student_id)]

absence_dt = absence_dt[,head(.SD,1), by=.(institution_id, course_id, student_id)]


####
# Gender
####
gender_dt = fread("data/student_grades_gender.csv")
gender_dt[, ':='(grade_id = NULL, course_name = NULL, course_subject = NULL, grade_name = NULL,
                 grade_abrreviation = NULL, country_name = NULL),]

gen_new = gender_dt[, lapply(.SD, as.numeric),]

# Merge absence and genders.
gen_ab = merge(gen_new, absence_dt, by = c("student_id", "institution_id", "course_id"))
gen_ab[,':='(course_id = NULL, grade_value = NULL, 
             AB = `AT Director` + `AT Home` + `AT No Reason` + `AT Other Reason` + `AT Sick`),]
gen_ab = unique(gen_ab)
gen_ab = gen_ab[, c(1,2,3,4,10),]
gen_ab[, AB := sum(AB), by = .(student_id, institution_id, gender, term_id)]
gen_ab = unique(gen_ab)
