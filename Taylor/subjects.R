# Associations Between Subjects
library(data.table)
library(ggplot2)
source("Taylor/data_prep.R")

# Read in student level data.
students = fread("data/student_grades.csv")
setkey(students, student_id, term_id)
  # Merge in english subjects
stud = merge(students, subs, by = c("institution_id","term_id","course_id"), all.x=TRUE)
stud = stud[order(student_id, term_id),,]

stud[, ':='(grade_id = NULL, course_subject_ET = NULL, course_subject = NULL, course_id = NULL),]

schools = unique(stud$institution_id)
# All possible grade combinations:
grades = expand.grid(1:5, 1:5)
grades_nofail = expand.grid(3:5, 3:5)

# Prep the contingency table
# failing = TRUE is all grades (1-5)
# failing = FALSE is just grades 3-5
prep_table = function(dt, failing) {
  # Shift to long
  dt_wide = dcast(dt, student_id ~ course_subject_EN, value.var = "grade_value",
                  fun.aggregate = sum)
  ## Some students didn't take both classes, remove them.
  dt_wide = na.omit(dt_wide)
  # If we don't want failing grades, subset data
  if(!failing) {
    dt_wide = dt_wide[unlist(dt_wide[,2]) >= 3 & unlist(dt_wide[,3]) >= 3,,]
  }
  # Make a table
  tab = table(unlist(dt_wide[,2]), unlist(dt_wide[,3]))
  # Fill in any missing grade combinations
  df = as.data.frame.table(tab)
  if(failing){
    df = merge(grades, df, by = c("Var1", "Var2"), all.x=TRUE)
  }
  else{
    df = merge(grades_nofail, df, by = c("Var1", "Var2"), all.x=TRUE)
  }
  df[is.na(df$Freq), "Freq"] = 0
  # Back to table
  if(failing){
    tab2 = matrix(df$Freq, nrow=5)
    rownames(tab2) = c("1", "2", "3", "4", "5")
    colnames(tab2) = c("1", "2", "3", "4", "5")
    tab2 = as.table(tab2)
  }
  else {
    tab2 = matrix(df$Freq, nrow=3)
    rownames(tab2) = c("3", "4", "5")
    colnames(tab2) = c("3", "4", "5")
    tab2 = as.table(tab2)
  }
  return(tab2)
}
# Run linear trend test on a table.
# Failing represents if you want grades 1 and 2 (failing = TRUE)
# Or just 3,4,5 (failing = FALSE)
ord_test = function(tab, failing) {
  n = sum(tab)
  px = margin.table(tab, 1) / n
  py = margin.table(tab, 2) / n
  # If we are including failing grades or not.
  if(failing) {
    X = 1:5
    Y = 1:5
  }
  else {
    X = 1:3
    Y = 1:3
  }
  Xbar = sum(X*px)
  Ybar = sum(Y*py)
  
  cov = sum(outer(X - Xbar, Y - Ybar) * prop.table(tab))
  vX = sum((X-Xbar)^2*px)
  vY = sum((Y-Ybar)^2*py)
  
  r = cov / sqrt(vX) / sqrt(vY)
  M2 = r^2 * (n-1)
  p = 1 - pchisq(M2, 1)
  
  return(list(r = r, p_value = p))
}


# For each possible combination of courses, need to subset data to just include those.
# It is the same course, p-value is going to be 0. They are obviously not independent.
l_dfs = list()
for(s in schools) {
  school_df = stud[institution_id == s,,]
  terms = unique(school_df$term_id)
  for(t in terms) {
    # Only data for that term.
    term_df = school_df[term_id == t,,]
    # List of unique courses already made in the data_prep file.
    # Make a dataframe with all combinations of students
    courses_expand = expand.grid(unique(term_df$course_subject_EN), unique(term_df$course_subject_EN))
    final_df = data.table(school = rep(s, nrow(courses_expand)),
                          term = rep(t, nrow(courses_expand)))
    # Data table to hold results
    courses_df = data.table(Course.1 = as.character(courses_expand$Var1),
                            Course.2 = as.character(courses_expand$Var2),
                            p_value = rep(NA, nrow(courses_expand)),
                            r = rep(NA, nrow(courses_expand)),
                            p_value_nofail = rep(NA, nrow(courses_expand)),
                            r_nofail = rep(NA, nrow(courses_expand)),
                            stringsAsFactors = FALSE)
    # Go over each course pair
    for(i in 1:nrow(courses_expand)) {
      # If the courses are the same, we know the answer.
      if(courses_df$Course.1[i] == courses_df$Course.2[i]) {
        courses_df$p_value[i] = 0
        courses_df$r[i] = 1
        courses_df$p_value_nofail[i] = 0
        courses_df$r_nofail[i] = 1
      } else {
        # Get data only for those two courses.
        temp = term_df[course_subject_EN == courses_df$Course.1[i] | 
                        course_subject_EN == courses_df$Course.2[i],
                      .(student_id, grade_value, course_subject_EN),]
        # Get table
        tab = prep_table(temp, failing=TRUE)
        tab_nofail = prep_table(temp, failing = FALSE)
        
        # Now run the ordinal test for linear independence
        results = ord_test(tab, failing=TRUE)  
        results_nofail = ord_test(tab_nofail, failing=FALSE)
        # Store results.
        courses_df$p_value[i] = results$p_value
        courses_df$r[i] = results$r
        courses_df$p_value_nofail[i] = results_nofail$p_value
        courses_df$r_nofail[i] = results_nofail$r

      }
    } # i for loop
    final_df = cbind(final_df, courses_df)
    l_dfs[[paste0(s,"-",t)]] = final_df
  } # term for loop
} # school for loop

fin_dt = rbindlist(l_dfs)
write.csv(fin_dt, "data/subject_associations.csv")

l_plots = list()
plot_types = c("p_value", "r", "p_value_nofail", "r_nofail")
# For each school
for(s in schools) {
  school_dt = fin_dt[school == s,,]
  terms = unique(school_dt$term)
  # For each term in that school
  for(t in terms) {
    term_dt = school_dt[term == t,,]
    # For each type of plot
    for(p in plot_types){
      if(grepl("nofail", p)) {
        f = FALSE
      } 
      else{
        f = TRUE
      }
      m = matrix(term_dt[,get(p),], nrow = length(unique(term_dt$Course.1)))
      m[upper.tri(m)] = NA
      colnames(m) = term_dt[1:length(unique(term_dt$Course.1)), Course.1,]
      rownames(m) = term_dt[1:length(unique(term_dt$Course.1)), Course.1,]
      m_melt = melt(m)
      # Plot it
      plt = ggplot(m_melt, aes(Var1, Var2)) + geom_tile(aes(fill= value), color='white') +
        theme_minimal() + 
        coord_fixed(ratio=1) + 
        scale_y_discrete(position="right") +
        theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              panel.grid.major=element_blank(),
              legend.position=c(0.1,0.9),
              legend.justification=c(0,1)) +
        ggtitle(paste0("School: ", s, ". Term: ", t, ". Failing Grades: ", f))
      
      # if this is a correalation plot
      if(grepl("r", p)) {
        plt = plt + scale_fill_distiller(limits=c(-1, 1), palette='RdBu', na.value='white',
                             name='Correlation')
      }
      else{
#        mx = max(term_dt[,get(p),])
        plt = plt + scale_fill_distiller(limits=c(0,1), palette='RdBu', na.value='white',
                                         name='P-Value')
      }
      l_plots[[paste0(s, "-", t, "-", p)]] = plt
    } # p for loop
  } # term for loop
} # school for loop

