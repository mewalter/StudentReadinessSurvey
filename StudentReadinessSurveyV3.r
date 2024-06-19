
library("tidyverse")
library("openxlsx")    # library("readxl") # this is the tidyverse installed package
library("scales")
library("lubridate")
library("rstudioapi")
library("jsonlite")
library("lessR")
library("likert")

BaseDir <- dirname(rstudioapi::getActiveDocumentContext()$path)
DataDir <- paste0(BaseDir,"/Data/")
#setwd(BaseDir)
#if( .Platform$OS.type == "unix" )
#  DataDir <- "~/Dropbox/Classes/MAE151F20/Grades/PeerEval/MidQuarter/Data"

# rm(list=ls())

# These are the CSV file labels for Google Forms/Surveys that are class-specific (e.g., ICS53 and MAE80)
names_labels3 <- c("submit","email","transfer","start",
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
                  "class")
names_vars3 <- c("submit","email","transfer","start","X1.1","X2.1","X3.1","X1.2","X2.2","X3.2","X1.3","X2.3","X3.3",
                 "X1.4.1","X1.4.2","X2.4","X3.4","X3.5","X3.6","class")


# These are the CSV file labels for Google Forms/Surveys that are from canvas quizzes (e.g., Engr190W) ... BEFORE extra columns are removed
names_canvaslabels <- c("name","section","submit","attempt","transfer","pts1",
                        "A. I understand the subject matter being taught in this course.","pts2",
                        "B. I believe the subject matter taught in this course is relevant to my career goals.","pts3",
                        "C. There are no out-of-the classroom issues or distractions affecting my ability to understand/achieve the student learning objectives of this course.","pts4",
                        "D. What I am learning in class has prepared me to complete the class assignments.","pts5",
                        "E. I believe there is value in the assignments to practice application of the course subject matter.","pts6",
                        "F. I know how to overcome non-academic issues that could hinder my ability to understand/achieve the student learning objectives of this course.","pts7",
                        "G. I can extend what I am learning in this class to new types of problems or new areas.","pts8",
                        "H. I believe there is value in retaining knowledge from this course for use in future endeavors.","pts9",
                        "I. I am not having to sacrifice other priorities to do well in this course.","pts10",
                        "J. I am confident that I will retain knowledge from this course to use in future endeavors.","pts11",
                        "K. I can show others how to apply or use the topics taught in this course.","pts12",
                        "L. I believe there is value in discussing or working through the course materials with others.","pts13",
                        "M. This course is well organized.","pts14",
                        "N. The instructor for this course is effective at teaching the course subject matter.","pts15",
                        "O. I am confident that my work is be evaluated fairly.","pts16",
                        "correct","incorrect","score","class")
names_canvasvars <- c("name","section","submit","attempt","transfer","pts1","X1.1","pts2","X2.1","pts3","X3.1","pts4","X1.2",
                      "pts5","X2.2","pts6","X3.2","pts7","X1.3","pts8","X2.3","pts9","X3.3","pts10","X1.4.1","pts11","X1.4.2",
                      "pts12","X2.4","pts13","X3.4","pts14","X3.5","pts15","X3.6","pts16","correct","incorrect","score","class")

# These are the CSV file labels for Google Forms/Surveys that are from canvas quizzes (e.g., Engr190W) ... AFTER extra columns are removed
names_vars4 <- c("name","transfer","X1.1","X2.1","X3.1","X1.2","X2.2","X3.2","X1.3","X2.3","X3.3","X1.4.1",
                 "X1.4.2","X2.4","X3.4","X3.5","X3.6","class")
names_labels4 <- c("name","transfer",
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
                        "class")

scale <- c("Strongly disagree", "Disagree", "Slightly Disagree", "Slightly Agree", "Agree", "Strongly agree")

source("SRSFunctions.R")

filenames_ICS <- list.files(path="./Data",pattern="ICS53",full.names=TRUE)
filenames_MAE80 <- list.files(path="./Data",pattern="MAE80",full.names=TRUE)
filenames_190W <- list.files(path="./Data",pattern="190W",full.names=TRUE)

# Class Data (All from Spring 2024)
ICS53_data1 <- read_raw_survey(filenames_ICS[1],names_vars3,"ICS53Sp24_1")
ICS53_data2 <- read_raw_survey(filenames_ICS[2],names_vars3,"ICS53Sp24_2")
ICS53_data3 <- read_raw_survey(filenames_ICS[3],names_vars3,"ICS53Sp24_3")

MAE80_data1 <- read_raw_survey(filenames_MAE80[1],names_vars3,"MAE80Sp24_1")
MAE80_data2 <- read_raw_survey(filenames_MAE80[2],names_vars3,"MAE80Sp24_2")

# get rid of the extra columns from the canvas quiz
temp <- read_raw_survey(filenames_190W[1],names_canvasvars,"Engr190WSp24_1")
Engr190W_data1 <- temp %>% select(starts_with("nam"),starts_with("transfer")|starts_with("X")|starts_with("clas")) %>% na.omit
temp <- read_raw_survey(filenames_190W[2],names_canvasvars,"Engr190WSp24_2")
Engr190W_data2 <- temp %>% select(starts_with("nam"),starts_with("transfer")|starts_with("X")|starts_with("clas")) %>% na.omit
temp <- read_raw_survey(filenames_190W[3],names_canvasvars,"Engr190WSp24_3")
Engr190W_data3 <- temp %>% select(starts_with("nam"),starts_with("transfer")|starts_with("X")|starts_with("clas")) %>% na.omit
temp <- read_raw_survey(filenames_190W[4],names_canvasvars,"Engr190WSp24_4")
Engr190W_data4 <- temp %>% select(starts_with("nam"),starts_with("transfer")|starts_with("X")|starts_with("clas")) %>% na.omit
temp <- read_raw_survey(filenames_190W[5],names_canvasvars,"Engr190WSp24_5")
Engr190W_data5 <- temp %>% select(starts_with("nam"),starts_with("transfer")|starts_with("X")|starts_with("clas")) %>% na.omit


ICS53_tcount1 <- count_transfers(ICS53_data1)
ICS53_tcount2 <- count_transfers(ICS53_data2)
ICS53_tcount3 <- count_transfers(ICS53_data3)
MAE80_tcount1 <- count_transfers(MAE80_data1)
MAE80_tcount2 <- count_transfers(MAE80_data2)
Engr190W_tcount1 <- count_transfers(Engr190W_data1)
Engr190W_tcount2 <- count_transfers(Engr190W_data2)
Engr190W_tcount3 <- count_transfers(Engr190W_data3)
Engr190W_tcount4 <- count_transfers(Engr190W_data4)
Engr190W_tcount5 <- count_transfers(Engr190W_data5)


# Survey results put into ordered factors
ICS53_factordata1 <- factor_data(ICS53_data1,names_vars3,names_labels3)
ICS53_factordata2 <- factor_data(ICS53_data2,names_vars3,names_labels3)
ICS53_factordata3 <- factor_data(ICS53_data3,names_vars3,names_labels3)
MAE80_factordata1 <- factor_data(MAE80_data1,names_vars3,names_labels3)
MAE80_factordata2 <- factor_data(MAE80_data2,names_vars3,names_labels3)
Engr190W_factordata1 <- factor_data(Engr190W_data1,names_vars4,names_labels4)
Engr190W_factordata2 <- factor_data(Engr190W_data2,names_vars4,names_labels4)
Engr190W_factordata3 <- factor_data(Engr190W_data3,names_vars4,names_labels4)
Engr190W_factordata4 <- factor_data(Engr190W_data4,names_vars4,names_labels4)
Engr190W_factordata5 <- factor_data(Engr190W_data5,names_vars4,names_labels4)

# combine factordata for various classes and show total resondents for each class
ICS53_factordata_all <- bind_rows(ICS53_factordata1,ICS53_factordata2,ICS53_factordata3)
ICS53Count <- ICS53_factordata_all %>% dplyr::count(class, name = 'Totals') 
MAE80_factordata_all <- bind_rows(MAE80_factordata1,MAE80_factordata2)
MAE80Count <- MAE80_factordata_all %>% dplyr::count(class, name = 'Totals') 
Engr190W_factordata_all <- bind_rows(Engr190W_factordata1,Engr190W_factordata2,Engr190W_factordata3,
                                     Engr190W_factordata4,Engr190W_factordata5)
Engr190Wcount <- Engr190W_factordata_all %>% dplyr::count(class, name = 'Totals') 


# look at one class  ... set to US Letter and Landscape
p <- likert(ICS53_factordata1[,c(5:19)])
ICS53_factordata1 %>% dplyr::count(class, name = 'Totals')
pdf("ICS53Survey1.pdf",width=8.5,height=11)
plot(p, legend.position="top", wrap=50, ordered=FALSE, group.order=names_labels3[5:19]) + ggtitle("ICS53 Survey #1 (164 Respondents)") 
dev.off()

# Look at transfer vesus non-transfer for one class ... set to portrait with 8.5 x 15
p <- likert(ICS53_factordata1[,c(5:19)], grouping = ICS53_factordata1[,3])
ICS53_tcount1
pdf("ICSSurvey1_Transfers.pdf",width=8.5,height=20)
plot(p, legend.position="top", wrap=90) + ggtitle("ICS53 Survey #1 (115 Non-Transfers and 49 Transfers)") 
dev.off()

# Multiple classes that were bind_rows  ... set to portrait with 8.5 x 20 (or more)
p <- likert(ICS53_factordata_all[,c(5:19)], grouping = ICS53_factordata_all[,20])
ICS53Count
pdf("ICS53AllSurveys.pdf",width=8.5,height=24)
plot(p, legend.position="top", wrap=90) + ggtitle("ICS53 Survey #1-3 (164, 154, 189 Respondents)") 
dev.off()
p <- likert(ICS53_factordata_all[,c(5:19)], grouping = ICS53_factordata_all[,3]) #transfers 
ICS53_tcount1
ICS53_tcount2
ICS53_tcount3
pdf("ICS53All_Transfers.pdf",width=8.5,height=20)
plot(p, legend.position="top", wrap=90) + ggtitle("ICS53 Transfers over All Surveys (115/49, 112/42, 1323/57)") 
dev.off()

# all of MAE80
p <- likert(MAE80_factordata_all[,c(5:19)], grouping = MAE80_factordata_all[,20])
MAE80Count
pdf("MAE80AllSurveys.pdf",width=8.5,height=24)
plot(p, legend.position="top", wrap=90) + ggtitle("MAE80 Survey #1 & #2 (79 and 64 Respondents)")
dev.off()
p <- likert(MAE80_factordata_all[,c(5:19)], grouping = MAE80_factordata_all[,3]) #transfers 
MAE80_tcount1
MAE80_tcount2
pdf("MAE80All_Transfers.pdf",width=8.5,height=20)
plot(p, legend.position="top", wrap=90) + ggtitle("MAE80 Transfers over All Surveys (68/11 and 57/7)") 
dev.off()

# all of Engr190W
p <- likert(Engr190W_factordata_all[,c(3:17)], grouping = Engr190W_factordata_all[,18])
Engr190Wcount
pdf("Engr190WAllSurveys.pdf",width=8.5,height=24)
plot(p, legend.position="top", wrap=90) + ggtitle("EngrMAE190W Surveys #1-#5 (29, 29, 30, 31, 20 Respondents)")  
dev.off()


############# now setup for the Winter 2024 data ###################

namebc <- names_survey_ref_bc()
nameg <- names_survey_ref_g()

filename_refb <- list.files(path="./Data",pattern="refb",full.names=TRUE)
alldatab <- read.csv(filename_refb, col.names = namebc$vars)

filename_refc <- list.files(path="./Data",pattern="refc",full.names=TRUE)
alldatac <- read.csv(filename_refc, col.names = namebc$vars)

filename_refg <- list.files(path="./Data",pattern="refg",full.names=TRUE)
alldatag <- read.csv(filename_refg, col.names = nameg$vars)
alldatagmod <- alldatag %>% select(starts_with("sub"),starts_with("emai")|starts_with("clas")|starts_with("X")) %>% 
  na.omit

classcountsb <- alldatab %>% dplyr:: count(class, name = 'N') %>% mutate(across('class',str_replace,'EngrMAE151A','EngrMAE151A1')) 
classcountsc <- alldatac %>% dplyr:: count(class, name = 'N') %>% mutate(across('class',str_replace,'EngrMAE151A','EngrMAE151A2')) 
classcountsg <- alldatag %>% dplyr:: count(class, name = 'N') 
classcountsgtran <- alldatag %>% dplyr:: count(transfer, name = 'N') 

pdf("RespondentTotalsRevB.pdf",paper="usr")
ggplot(data=classcountsb,aes(x=class, y=N, fill=class)) + geom_bar(stat="identity") + 
  geom_text(aes(label = N), vjust = -0.2) +
  ggtitle("Number of Students Responding per Class (RevB") + xlab("Class") + ylab("Number of Students") +
  theme(legend.position = "none", axis.text.x = element_text(angle=45,hjust=1),axis.text.y=element_text(angle=90))
dev.off()

pdf("RespondentTotalsRevG.pdf",paper="usr")
ggplot(data=classcountsg,aes(x=class, y=N, fill=class)) + geom_bar(stat="identity") + 
  geom_text(aes(label = N), vjust = -0.2) +
  ggtitle("Number of Students Responding per Class (RevG)") + xlab("Class") + ylab("Number of Students") +
  theme(legend.position = "none", axis.text.x = element_text(angle=45,hjust=1),axis.text.y=element_text(angle=90))
dev.off()

# Survey B: 151A1, 130B, 152, 80, 106
factordatab <- alldatab %>% mutate_at(vars(starts_with("X")), funs(factor(., levels = scale, ordered = TRUE))) %>% 
  mutate(across('class',str_replace,'EngrMAE151A','EngrMAE151A1')) %>% 
  rename_at(vars(namebc$vars), ~ namebc$labels)

# high responses from Survey B
factordatab151a130b <- factordatab %>% filter(class=="EngrMAE151A1"|class=="EngrMAE130B")  

# Survey C: 151A2
factordatac <- alldatac %>% mutate_at(vars(starts_with("X")), funs(factor(., levels = scale, ordered = TRUE))) %>% 
  mutate(across('class',str_replace,'EngrMAE151A','EngrMAE151A2')) %>% rename_at(vars(namebc$vars), ~ namebc$labels)

# Survey G (pick your hardest class)
factordatag <- alldatag %>% mutate_at(vars(starts_with("X")), funs(factor(., levels = scale, ordered = TRUE))) %>% 
  rename_at(vars(nameg$vars), ~ nameg$labels)
factordatagmod <- alldatagmod %>% mutate_at(vars(starts_with("X")), funs(factor(., levels = scale, ordered = TRUE))) %>% 
  rename_at(vars(namebc$vars), ~ namebc$labels)

# Just 151A1 and 151A2 data
factordatab151a <- factordatab %>% filter(class=="EngrMAE151A1")  
factordatabc <- bind_rows(factordatab151a,factordatac)
factordatabcg <- bind_rows(factordatabc,factordatagmod)
factordatabcg151 <- factordatabcg %>% filter(class=="EngrMAE151A1"|class=="EngrMAE151A2"|class=="EngrMAE151A3")
classcountsBCG151 <- factordatabcg151 %>% dplyr:: count(class, name = 'N') 


# Plots
# All Survey B data ... includes low numbers of 80, 106, and 152 students
p <- likert(factordatab[,c(4:18)], grouping = factordatab[,3])
pdf("SurveyBAll.pdf",width=8.5,height=24)
plot(p, legend.position="top", wrap=90) + ggtitle("Survey B (Wi24) All Responses") 
dev.off()

# All high response Survey B data ... just for 151A1 and 130B
p <- likert(factordatab151a130b[,c(4:18)], grouping = factordatab151a130b[,3])
pdf("SurveyBHighResponse.pdf",width=8.5,height=24)
plot(p, legend.position="top", wrap=90) + ggtitle("Survey B (Wi24) EngrMAE151A1 and EngrMAE130B") 
dev.off()

# Compare 151A1 and 151A2 ... just the two 151 surveys
p <- likert(factordatabc[,c(4:18)], grouping = factordatabc[,3])
classcountsb
classcountsc
pdf("Survey151A1-2.pdf",width=8.5,height=24)
plot(p, legend.position="top", wrap=90) + ggtitle("Survey B&C (Wi24) EngrMAE151A1 (122) and EngrMAE151A2 (113)")
dev.off()

# Look at transfer versus non-transfer from RevG
p <- likert(factordatag[,c(8:22)], grouping = factordatag[,3])
classcountsgtran
pdf("SurveyGtransfer.pdf",width=8.5,height=24)
plot(p, legend.position="top", wrap=90) + ggtitle("Survey G (Wi24) Transfer (46) and Non-Transfer (26)") 
dev.off()

p <- likert(factordatagmod[,c(4:18)], grouping = factordatagmod[,3])
pdf("SurveyGAll.pdf",width=8.5,height=34)
plot(p, legend.position="top", wrap=90) + ggtitle("Survey G (Wi24) All Respondents")
dev.off()

p <- likert(factordatabcg[,c(4:18)], grouping = factordatabcg[,3])
pdf("SurveyBCG_All.pdf",width=8.5,height=34)
plot(p, legend.position="top", wrap=90) + ggtitle("Survey G (Wi24) All Respondents")
dev.off()

p <- likert(factordatabcg151[,c(4:18)], grouping = factordatabcg151[,3])
classcountsBCG151
pdf("SurveyBCG_151.pdf",width=8.5,height=34)
plot(p, legend.position="top", wrap=90) + ggtitle("EngrMAE151 Survey #1-#3 with 122, 113, 37")
dev.off()

pdf("SurveyGCompletionTimeWi24.pdf",paper="us")
ggplot(factordatag, aes(x=factor(0), y=completed)) + 
  geom_boxplot(outlier.colour = "red",outlier.shape = 1,fill = "white",colour = "#3366FF") +
  ggtitle("Self-Reported Time to Complete Survey G (72 Respondants)") + 
  labs(x="Response Time",y="Time (min)") + scale_y_continuous(breaks=seq(0,20,by=2)) 
dev.off()


################# need to get survey completion data from ICS53, and MAE80 data

finish <- rbind(ICS53_factordata_all$submit,MAE80_factordata_all$submit)
start <- rbind(ICS53_factordata_all$start,MAE80_factordata_all$start)
finish_times <- parse_date_time(sub("^\\S+\\s+", '', finish),'%H:%M:%S')
start_times <- parse_date_time(start,'%I:%M:%S %p')
total_time <- difftime(finish_times, start_times, units='mins')
total_time_removed <- total_time[(total_time>0&total_time<20)]

pdf("SurveyCompletionTimeSp24.pdf",paper="us")
ggplot(factordatag, aes(x=factor(0), y=completed)) + 
  geom_boxplot(outlier.colour = "red",outlier.shape = 1,fill = "white",colour = "#3366FF") +
  ggtitle("Semi-Self-Reported Time to Complete ICE53 and MAE80 Surveys (950 Surveys)") + 
  labs(x="Response Time",y="Time (min)") + scale_y_continuous(breaks=seq(0,20,by=2)) 
dev.off()



################# Misc things I was playing with ####################



boxplot(factordatag$completet)

+ 
  theme(legend.position="top", legend.text = element_text(size = rel(0.2)))


group.order=names_labels[4:18]) 

p <- likert(survey4)
a <- likert.bar.plot(p, legend.position = "right", text.size = 4) +
  theme(text = element_text(size = rel(4)),axis.text.y = element_text(size = rel(2))) +
  theme_update(legend.text = element_text(size = rel(0.7))) +
  theme_classic()
plot(a)




write.csv(teamdata, file = "../GroupsWithNames.csv",row.names=FALSE)


file_list <- list.files()
# delete some files  
file_list <- file_list[!grepl(paste0("desktop.ini", collapse = "|"), file_list)]


