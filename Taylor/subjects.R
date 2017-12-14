# Associations Between Subjects
library(data.table)
library(ggplot2)
library(gridExtra)
library(grid)
source("Taylor/data_prep.R")

# Read in student level data.
students = fread("data/student_grades.csv")
students[, ':='(grade_id = as.numeric(grade_id),
                student_id = as.numeric(student_id),
                institution_id = as.numeric(institution_id),
                term_id = as.numeric(term_id),
                course_id = as.numeric(course_id),
                grade_value = as.numeric(grade_value))]
setkey(students, student_id, term_id)

# Merge in english subjects
# Subs was made in data_prep
stud = merge(students, subs, by = c("institution_id","term_id","course_id"), all.x=TRUE)
stud = stud[order(student_id, term_id),,]
stud[, ':='(grade_id = NULL, course_subject_ET = NULL, course_subject = NULL),]
schools = unique(stud$institution_id)
# All possible grade combinations:
# This will be used to fill in any missing grades (No one got a 2 in biology)
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

# Save the data so you don't have to rerun everythin every time.
#write.csv(fin_dt, "data/subject_associations.csv")

##########################################
# Make the plots
#########################################
l_plots = list()
plot_types = c("p_value", "r")
# For each school
first = TRUE

# For the first plot we want the legend. 
# Couldn't figure out how to do it dynamically.
school_dt = fin_dt[school == 524 & Course.1 %in% courses & Course.2 %in% courses,,]
term_dt = school_dt[term == 13792361748,,]
term_dt = term_dt[order(Course.2, Course.1),,]
m = matrix(term_dt[,get(p),], nrow = length(unique(term_dt$Course.1)))
m[upper.tri(m)] = NA
colnames(m) = term_dt[1:length(unique(term_dt$Course.1)), Course.1,]
rownames(m) = term_dt[1:length(unique(term_dt$Course.1)), Course.1,]
m_melt = melt(m)

plt = ggplot(m_melt, aes(Var1, Var2)) + geom_tile(aes(fill= value), color='white') +
  theme_minimal() + 
  coord_fixed(ratio=1) + 
  scale_y_discrete(position="right") +
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major=element_blank(),
        legend.position=c(0.05,0.95),
        legend.justification=c(0,1)) +
  ggtitle(paste0("School: ", 524, ". Term: ", 13792361748)) + 
  scale_fill_distiller(limits=c(-1, 1), palette='RdBu', na.value='white',
                       name='Correlation')

l_plots[["First"]] = plt

# For all of the other schools, make the plots
for(s in schools) {
  school_dt = fin_dt[school == s & Course.1 %in% courses & Course.2 %in% courses,,]
  terms = unique(school_dt$term)
  # For each term in that school
  for(t in terms) {
    term_dt = school_dt[term == t,,]
    term_dt = term_dt[order(Course.2, Course.1),,]
    # For each type of plot
    for(p in plot_types){
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
              panel.grid.major=element_blank()) +
        ggtitle(paste0("School: ", s, ". Term: ", t)) + 
        scale_fill_distiller(limits=c(-1, 1), palette='RdBu', na.value='white',
                             name='Correlation') +
        guides(fill=FALSE)
      
#       # if this is a correalation plot
#       if(grepl("r", p)) {
#         plt = plt + scale_fill_distiller(limits=c(-1, 1), palette='RdBu', na.value='white',
#                                        name='Correlation')
#       }
#       else{
# #        mx = max(term_dt[,get(p),])
#         plt = plt + scale_fill_distiller(limits=c(0,1), palette='RdBu', na.value='white',
#                                          name='P-Value')
#       }
      l_plots[[paste0(s, "-", t)]] = plt
    } # p for loop
  } # term for loop
} # school for loop


# Create layout for the plot.
lay <- rbind(c(NA,1,2,NA),
             c(3,3,4,5),
             c(6,7,8,9))
grid.arrange(l_plots[[1]], l_plots[[3]], l_plots[[4]],
               l_plots[[5]], l_plots[[6]], l_plots[[7]],
               l_plots[[8]], l_plots[[9]], l_plots[[10]], layout_matrix=lay)

# Individual schools plots
grid.arrange(l_plots[[1]], l_plots[[3]],nrow=1)
grid.arrange(l_plots[[4]],
             l_plots[[5]], l_plots[[6]],nrow=1)
grid.arrange(l_plots[[7]],
             l_plots[[8]], l_plots[[9]], l_plots[[10]],nrow=2)

######
# Which tests were significant?
######
s = copy(fin_dt[Course.1 %in% courses & Course.2 %in% courses, .(school, term, Course.1, Course.2, p_value, r),])
s[,':='(V1 = gsub(" ", "", Course.1, fixed = TRUE),V2 = gsub(" ", "", Course.2, fixed = TRUE)),]
s[,':='(V1 = tolower(V1),V2 = tolower(V2)),]
s[, comb := paste0(V1, V2),]

f = Vectorize(function(x) {paste(sort(unlist(strsplit(x, ""))), collapse = "")})

s[, comb := f(comb),]

un = s[Course.1 != Course.2,]
un = un[, head(.SD, 1), by = .(school, term, p_value, r, comb)]

sig_tests = un[p_value >= .05,]

sig_tests[order(Course.1),.(school,term,p_value,Course.1,Course.2),]

summary(factor(sig_tests$Course.1))
summary(factor(sig_tests$Course.2))
summary(factor(sig_tests$school))
##########################
# Are there types of students?
# Do better in one subject or another.
# Run PCA!
##############################
stud2 = stud[course_subject_EN %in% courses,,]
stud_wide = dcast(stud2, institution_id + term_id + student_id ~ course_subject_EN, 
                  value.var="grade_value",fun.aggregate = sum)

source("Taylor/gender_abs.R")

grade_abs_stud = merge(stud_wide, gen_ab, by=c("student_id", "institution_id", 
                                                 "term_id"))

grade_abs_stud = unique(grade_abs_stud)
stud_quant = grade_abs_stud[, 4:(ncol(grade_abs_stud)),]
stud_quant = scale(stud_quant)
stud_pca = prcomp(stud_quant)

summary(stud_pca)
screeplot(stud_pca) # First PCA does most of the explainin
biplot(stud_pca)
