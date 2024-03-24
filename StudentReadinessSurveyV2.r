
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
setwd(BaseDir)
#if( .Platform$OS.type == "unix" )
#  DataDir <- "~/Dropbox/Classes/MAE151F20/Grades/PeerEval/MidQuarter/Data"

# rm(list=ls())

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

name_vec = data.frame("vars"=names_vars,"labels"=names_labels)

names_labels2 <- c("submit","email","transfer","quarter","dept","course","startt",
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
                  "completet")
names_vars2 <- c("submit","email","transfer","quarter","dept","course","startt","X1.1","X2.1","X3.1","X1.2","X2.2","X3.2","X1.3","X2.3","X3.3",
                "X1.4.1","X1.4.2","X2.4","X3.4","X3.5","X3.6","completet")

name_vec2 = data.frame("vars"=names_vars2,"labels"=names_labels2)


scale <- c("Strongly disagree", "Disagree", "Slightly Disagree", "Slightly Agree", "Agree", "Strongly agree")

filename <- paste0(DataDir,"refbresponses20mar1256.csv")
alldatab <- read.csv(filename, col.names = names_vars)

filename <- paste0(DataDir,"refcresponses20mar1257.csv")
alldatac <- read.csv(filename, col.names = names_vars)

filename <- paste0(DataDir,"refgresponses20mar1258.csv")
alldatag <- read.csv(filename, col.names = names_vars2)


classcountsb <- alldatab %>% dplyr:: count(class, name = 'N') %>% mutate(across('class',str_replace,'EngMAE151A','EngMAE151A1')) 
classcountsc <- alldatac %>% dplyr:: count(class, name = 'N') %>% mutate(across('class',str_replace,'EngMAE151A','EngMAE151A2')) 
classcountsg <- alldatag %>% dplyr:: count(transfer, name = 'N') 

ggplot(data=classcountsb,aes(x=class, y=N, fill=class)) + geom_bar(stat="identity") + 
  geom_text(aes(label = N), vjust = -0.2) +
  ggtitle("Number of Students Responding per Class") + xlab("Class") + ylab("Number of Students") +
  theme(legend.position = "none", axis.text.x = element_text(angle=45,hjust=1),axis.text.y=element_text(angle=90))

# Survey B: 151A1, 130B, 152, 80, 106
factordatab <- alldatab %>% mutate_at(vars(starts_with("X")), funs(factor(., levels = scale, ordered = TRUE))) %>% 
  mutate(across('class',str_replace,'EngMAE151A','EngMAE151A1')) %>% rename_at(vars(name_vec$vars), ~ name_vec$labels)

# high responses from Survey B
factordatab151a130b <- factordatab %>% filter(class=="EngMAE151A1"|class=="EngrMAE130B")  

# Survey C: 151A2
factordatac <- alldatac %>% mutate_at(vars(starts_with("X")), funs(factor(., levels = scale, ordered = TRUE))) %>% 
  mutate(across('class',str_replace,'EngMAE151A','EngMAE151A2')) %>% rename_at(vars(name_vec$vars), ~ name_vec$labels)

# Survey G
factordatag <- alldatag %>% mutate_at(vars(starts_with("X")), funs(factor(., levels = scale, ordered = TRUE))) %>% 
#  mutate(across('class',str_replace,'EngMAE151A','EngMAE151A3')) %>% 
  rename_at(vars(name_vec2$vars), ~ name_vec2$labels)

# Just 151A1 and 151A2 data
factordatab151a <- factordatab %>% filter(class=="EngMAE151A1")  
factordatabc <- bind_rows(factordatab151a,factordatac)


# All Survey B data
p <- likert(factordatab[,c(4:18)], grouping = factordatab[,3])
plot(p, legend.position="top", wrap=90) 

# All high response Survey B data
p <- likert(factordatab151a130b[,c(4:18)], grouping = factordatab151a130b[,3])
plot(p, legend.position="top", wrap=90) 

# Compare 151A1 and 151A2
p <- likert(factordatabc[,c(4:18)], grouping = factordatabc[,3])
plot(p, legend.position="top", wrap=90) 

# Look at transfer vesus non-transfer
p <- likert(factordatag[,c(8:22)], grouping = factordatag[,3])
plot(p, legend.position="top", wrap=90) 

ggplot(factordatag, aes(x=factor(0), y=completet)) + 
  geom_boxplot(outlier.colour = "red",outlier.shape = 1,fill = "white",colour = "#3366FF") + # This is the plot function
  labs(x="Response Time",y="Time (min)") + scale_y_continuous(breaks=seq(0,20,by=2)) 



boxplot(factordatag$completet)



p <- likert(factordatac[,c(4:18)], grouping = factordatac[,3])
plot(p, legend.position="top", wrap=90) 






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

i <- 70       # for debugging
AllRankings = list()          # initialize list variable
for (i in 1:length(file_list)) {
  cat(file_list[i],sep='\n')
  FileData <- read.xlsx(file_list[i],colNames=FALSE)
  Comment.Row <- which(startsWith(FileData$X1,"Comments"))            # identify comment row
  NumMembers <- Comment.Row-7                                         # identify number of team members
  FileData.Comments <- FileData %>% slice((Comment.Row+1):(Comment.Row+NumMembers))        # collect comments
  FileData.Comments <- as.data.frame(t(apply(FileData.Comments, 1, function(x) x[order(is.na(x))])))   # shift so comments are in one column
  FileData.Rankings <- t(FileData %>% slice(1:(Comment.Row-1)) %>% select(1:(Comment.Row-7)))          # collect rankings (and transpose)
  FileData.Rankings <- as.data.frame(t(apply(FileData.Rankings, 1, function(x) x[order(is.na(x))])))   # shift rankings to eliminate offset
  FileData.Rankings <- FileData.Rankings %>% mutate_at(vars(V2,V3,V4,V5,V6),as.numeric) %>%            
    mutate(across(where(is.factor),as.character)) %>% 
    select(1:6)                              # change to numeric and character and select first 6 columns
  FileData.Rankings$Comments <- FileData.Comments$V1                  # insert the written comments
  FileData.Rankings$Rater <- gsub("_.*","",file_list[i])              # insert name of the person doing reviewing
  AllRankings[[i]] <- FileData.Rankings                               # add to master file
}   
FinalRankings <- dplyr::bind_rows(AllRankings)       # turn list into data frame
FinalRankings <- FinalRankings %>% arrange(desc(V1))   # just to dispay names together also has comments

# For each student: get relevant columns, get sum and means, rename columns
Results <- FinalRankings %>% select(1:6) %>% group_by(V1) %>% summarise_all(list(mean)) %>% 
  rowwise() %>% mutate(Sum=sum(c(V2,V3,V4,V5,V6),na.rm=T)) %>% mutate(Avg=mean(c(V2,V3,V4,V5,V6),na.rm=T)) %>%
  rename(Name=V1, Skill=V2, Contrib=V3, Teaming=V4, OnTrack=V5, Quality=V6)

# Load team data to get team number and ucnetID into Results file ... FUTURE convert "Team X" to just "X"
# teamdata = read.csv("../GroupsWithNames.csv", header = TRUE, stringsAsFactors = FALSE)
Results <- Results %>% left_join(teamdata, by="Name")  %>%  # does NOT remove duplicates
  rename(TeamNum=ProjectName, NumMem=NumMembers)
# Normalize average rating by team's max score
Results <- Results %>%  group_by(TeamNum) %>% mutate(TeamMax=max(Avg)) %>% ungroup() %>% arrange(TeamNum) %>%
  mutate(AvgNorm = Avg/TeamMax*5) 

# setwd(BaseDir)
write.csv(FinalRankings, file = "../AllRankingsComments.csv",row.names=FALSE)
write.csv(Results, file = "../Result4EachPerson.csv",row.names=FALSE)



