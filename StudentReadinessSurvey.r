
library("tidyverse")
library("openxlsx")    # library("readxl") # this is the tidyverse installed package
library("scales")
library("lubridate")
library("rstudioapi")
library("jsonlite")


######### NOTE: probably only works when there is only ONE group category ##################

# set the Canvas Class ID
#class_id <- "61600" # MAE151B_W24
class_id <- "60836" # MAE151A_W24


# set some strings for the fromJSON calls
token <- "4407~cV0DPpTSmVsjyrYteGHINIXvE76TD7RTy750ASCHFUfj6yqMONUXOqlgWsoPkIXt" #Authorization token. Set this up in your Canvas profile
canvas_base <- "https://canvas.eee.uci.edu/api/v1/"
cats_call <- paste0("/group_categories?access_token=",token)
groups_call <- paste0("/groups?per_page=100&access_token=",token)
users_call <- paste0("/users?per_page=100&access_token=",token)

#now get ids for each group category ########## ONLY ONE should exist ################
call4cats <- paste0(canvas_base,"courses/",class_id,cats_call)
categorydata <- fromJSON(call4cats)


#now find all the ids and names of each group/team in each category    ############## ONLY ONE Category!!! ##########
call4groups <- paste0(canvas_base,"group_categories/",categorydata$id,groups_call)
groupdata <- fromJSON(call4groups)
group_info <- tibble(GroupID=groupdata$id,GroupName=groupdata$name,MemberCnt=groupdata$members_count) %>% 
  filter(MemberCnt>0)    # drop any groups that have zero members

# now get set the call string for each group/team and then loop through and get data into "teamdata"
call4users <- paste0(canvas_base,"groups/",group_info$GroupID,users_call)  
teamdata <- tibble(ProjectName=character(), NumMembers=numeric(), UCInetID=character(), Name=character())
for (i in 1:nrow(group_info)) {
  userdata <- fromJSON(call4users[i])
  teamdata <- teamdata %>% add_row(ProjectName=group_info$GroupName[i],NumMembers=group_info$MemberCnt[i],
                   UCInetID=userdata$login_id,Name=userdata$name)
}

BaseDir <- setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#BaseDir <- setwd("~/pCloudDrive/RProjects/PeerEvalAnalysis")
DataDir <- paste0(BaseDir,"/Data/")
setwd(DataDir)
#if( .Platform$OS.type == "unix" )
#  DataDir <- "~/Dropbox/Classes/MAE151F20/Grades/PeerEval/MidQuarter/Data"

# rm(list=ls())

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



