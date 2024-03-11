
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

scale <- c("Strongly disagree", "Disagree", "Slightly Disagree", "Slightly Agree", "Agree", "Strongly agree")

filename <- paste0(DataDir,"refbresponses9mar1200.csv")

alldata <- read.csv(filename, col.names = names_vars)

classcounts <- alldata %>% dplyr:: count(class, name = 'N') 

ggplot(data=classcounts,aes(x=class, y=N, fill=class)) + geom_bar(stat="identity") + 
  geom_text(aes(label = N), vjust = -0.2) +
  ggtitle("Number of Students Responding per Class") + xlab("Class") + ylab("Number of Students") +
  theme(axis.text.x = element_text(angle=45,hjust=1),axis.text.y=element_text(angle=90))

#data151a <- alldata %>% filter(class=="EngMAE151A")

factordata <- alldata %>% mutate_at(vars(starts_with("X")), funs(factor(., levels = scale, ordered = TRUE))) %>% 
  rename_at(vars(name_vec$vars), ~ name_vec$labels)

# likert(factordata[,c(4:18)], grouping = factordata[,3])

#all questions
p <- plot(likert(factordata[,c(4:18)], grouping = factordata[,3]), legend.position="top", label_wrap_mod(value, width = 75))


p <- likert(factordata[,c(4:18)], grouping = factordata[,3])
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



