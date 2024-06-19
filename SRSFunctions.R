

read_raw_survey <- function(filename,column_names,class_name){
  dframe <- read.csv(filename, col.names = column_names, na.strings = c("", "NA")) 
  dframe$class <- class_name 
  return(dframe)
}


count_transfers <- function(dframe){
  tcount <- dframe %>% dplyr::count(transfer, name = 'N') 
  return(tcount)
  #classcounts <- alldata %>% dplyr:: count(class, name = 'N') %>% mutate(across('class',str_replace,'EngMAE151A','EngMAE151A1')) 
}


factor_data <- function(dframe,vars,labels){
  data <- dframe %>% mutate_at(vars(starts_with("X")), funs(factor(., levels = scale, ordered = TRUE))) %>% 
    rename_at(vars(vars), ~ labels)
}


names_survey_ref_bc <- function(){
  names_labels <- c("submit","email","class",
                    "A. I understand the subject matter being taught in this course.",
                    "B. I believe the subject matter taught in this course is relevant to my career goals.",
                    "C. There are no out-of-the classroom issues or distractions affecting my ability to understand/achieve the student learning objectives of this course.",
                    "D. What I am learning in class has prepared me to complete the class assignments.",
                    "E. I believe there is value in the assignments to practice application of the course subject matter.",
                    "F. I know how to overcome non-academic issues that could hinder my ability to understand/achieve the student learning objectives of this course.",
                    "G. I can extend what I am learning in this class to new types of problems or new areas.",
                    "H. I believe there is value in retaining knowledge from this course for use in future endeavors.",
                    "I. I am not having to sacrifice other priorities to do well in this course.",
                    "J. I am confident that I will retain knowledge from this course to use in future endeavors.",
                    "K. I can show others how to apply or use the topics taught in this course.",
                    "L. I believe there is value in discussing or working through the course materials with others.",
                    "M. This course is well organized.",
                    "N. The instructor for this course is effective at teaching the course subject matter.",
                    "O. I am confident that my work is be evaluated fairly.")
  names_vars <- c("submit","email","class","X1.1","X2.1","X3.1","X1.2","X2.2","X3.2","X1.3","X2.3","X3.3",
                  "X1.4.1","X1.4.2","X2.4","X3.4","X3.5","X3.6")
  names_vec = data.frame("vars"=names_vars,"labels"=names_labels)
  return(names_vec)
}

names_survey_ref_g <- function(){
  names_labels2 <- c("submit","email","transfer","quarter","dept","class","startt",
                     "A. I understand the subject matter being taught in this course.",
                     "B. I believe the subject matter taught in this course is relevant to my career goals.",
                     "C. There are no out-of-the classroom issues or distractions affecting my ability to understand/achieve the student learning objectives of this course.",
                     "D. What I am learning in class has prepared me to complete the class assignments.",
                     "E. I believe there is value in the assignments to practice application of the course subject matter.",
                     "F. I know how to overcome non-academic issues that could hinder my ability to understand/achieve the student learning objectives of this course.",
                     "G. I can extend what I am learning in this class to new types of problems or new areas.",
                     "H. I believe there is value in retaining knowledge from this course for use in future endeavors.",
                     "I. I am not having to sacrifice other priorities to do well in this course.",
                     "J. I am confident that I will retain knowledge from this course to use in future endeavors.",
                     "K. I can show others how to apply or use the topics taught in this course.",
                     "L. I believe there is value in discussing or working through the course materials with others.",
                     "M. This course is well organized.",
                     "N. The instructor for this course is effective at teaching the course subject matter.",
                     "O. I am confident that my work is be evaluated fairly.",
                     "completed")
  names_vars2 <- c("submit","email","transfer","quarter","dept","class","startt","X1.1","X2.1","X3.1","X1.2","X2.2","X3.2","X1.3","X2.3","X3.3",
                   "X1.4.1","X1.4.2","X2.4","X3.4","X3.5","X3.6","completed")
  name_vec2 = data.frame("vars"=names_vars2,"labels"=names_labels2)
  return(name_vec2)
}
  



