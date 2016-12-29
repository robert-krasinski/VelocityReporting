library("jsonlite")
library(ggplot2)
library(zoo)
library(reshape2)
library(lubridate)
library(viridis)
library(ggthemes)

rm(list=ls(all=TRUE)) 
#pallette
c25 <- c("#E31A1C", # red"dodgerblue2",
         "green4",
         "#6A3D9A", # purple
         "#FF7F00", # orange
         "black","gold1",
         "skyblue2","#FB9A99", # lt pink
         "palegreen2",
         "#CAB2D6", # lt purple
         "#FDBF6F", # lt orange
         "gray70", "khaki2",
         "maroon","orchid1","deeppink1","blue1","steelblue4",
         "darkturquoise","green1","yellow4","yellow3",
         "darkorange4","brown")


#json_file_versions <- "./data/VelocityIssues2016-08-17 08.42.49.522788.csv"
#versions <- stream_in(file(json_file_versions))

getLatestFile <- function(filePattern)
{
    files <- list.files(path = "./data/", pattern = filePattern)
    files <- sort(files, decreasing = TRUE)
    latestFile <- paste("./data/", files[1], sep = "")

    return(latestFile)
}

reviewersLatestFile <- getLatestFile("reviewers2016.*.csv")
reviewers <- read.csv(
  file= reviewersLatestFile,
  head=TRUE,sep=",", dec=".", stringsAsFactors=FALSE)
#View(reviewers)
#stop()

componentIssueLatestFile <- getLatestFile("ComponentIssues2016.*.csv")
componentIssue <- read.csv(
  file= componentIssueLatestFile,
  head=TRUE,sep=",", dec=".", stringsAsFactors=FALSE)
#View(componentIssue)
#stop()

#files <- list.files(path = "./data/", pattern = "VelocityIssues2016.*.csv")
#files <- sort(files, decreasing = TRUE)
#latestFile <- paste("./data/", files[1], sep = "")
latestFile <- getLatestFile("VelocityIssues2016.*.csv")

#stop()

issues <- read.csv(
    file= latestFile,
    head=TRUE,sep=",", dec=".", stringsAsFactors=FALSE)

#githubLatestFile <- getLatestFile('GitHubIssues2016.*.csv')

#gitHubComments <- read.csv(githubLatestFile,
  #file = "./data/GitHubIssues2016-08-02 09.25.14.408001.csv",
#  head=TRUE,sep=",", dec=".", stringsAsFactors=FALSE)


#gitHubComments$created <- as.POSIXct(gitHubComments$created, format="%Y-%m-%d %H:%M:%S")
#gitHubComments[, "createdMonth"] <- as.Date(cut(as.Date(gitHubComments$created), breaks = "month"))
#gitHubComments[, "createdWeek"] <- as.Date(cut(as.Date(gitHubComments$created), breaks = "week"))
#gitHubComments[, "Quantity"] <- 1

#View(gitHubComments)
#stop()


completedStatuses <- c('Completed', 'Rejected', 'Reviewed', 'Resolved', 'Closed', 'Awaiting Review')

minus3M <- as.Date(Sys.Date()) %m+% months(-3)

# issuesWithNoZeroCompleteTime <- issues[issues$timeToComplete > 0,]
# VELIssues <- issuesWithNoZeroCompleteTime[issuesWithNoZeroCompleteTime$project == 'VEL', ]
# VELIssuesCompleted <- VELIssues[VELIssues$status %in% completedStatuses,]
# VELIssuesWithoutLongest <- VELIssuesCompleted[VELIssuesCompleted$timeToComplete <= 541, ]
# VBSIssues <- issuesWithNoZeroCompleteTime[issuesWithNoZeroCompleteTime$project == 'VBS', ]
# VBSIssuesCompleted <- VBSIssues[VBSIssues$status %in% completedStatuses,]
# VBSIssuesWithoutLongest <- VBSIssuesCompleted[VBSIssuesCompleted$timeToComplete <= 366, ]
# 
# vbsQuantile <- quantile(VBSIssuesCompleted$timeToComplete, c(.5, .7, .8, .9))
# velQuantile <- quantile(VELIssuesCompleted$timeToComplete, c(.5, .7, .8, .9))
# 
# hist(VELIssues$timeToComplete)
# hist(VBSIssues$timeToComplete)
# hist(VELIssuesWithoutLongest$timeToComplete, breaks = 4)
# hist(VBSIssuesWithoutLongest$timeToComplete, breaks = 4)

#convert to datetime
issues$updated <- as.POSIXct(issues$updated, format="%Y-%m-%dT%H:%M:%S")
issues$created <- as.POSIXct(issues$created, format="%Y-%m-%dT%H:%M:%S")
issues$movedToComplete <- as.POSIXct(issues$movedToComplete, format="%Y-%m-%dT%H:%M:%S")
issues$workDayToComplete <- issues$workTimeToComplete / 7.5


issues[, "StartMonth"] <- as.Date(cut(as.Date(issues$created), breaks = "month"))
issues[, "EndMonth"] <- as.Date(cut(as.Date(issues$movedToComplete), breaks = "month")) 
issues[, "StartWeek"] <- as.Date(cut(as.Date(issues$created), breaks = "week"))
issues[, "EndWeek"] <- as.Date(cut(as.Date(issues$movedToComplete), breaks = "week"))
issues[, "Quantity"] <- 1

#issues <- issues[issues$type == 'Bug', ]

#time to complete week, month
finishedIssues <- issues[issues$status %in% completedStatuses,]
finishedIssues <- finishedIssues[!finishedIssues$type %in% c('Sub-task'),]

openIssues <- issues[!issues$status %in% completedStatuses,]
openIssues <- openIssues[!finishedIssues$type %in% c('Sub-task'),]
#View(openIssues)
#stop()



#-----------------------------------------------------------------------------------------------------
#download all components
componentsLatestFile <- getLatestFile('components_.*json')
componentsList <- read.csv(
  file= componentsLatestFile,
  head=TRUE,sep=",", dec=".", stringsAsFactors=FALSE)



#components and developers
components <- merge(finishedIssues, componentIssue, by=c("key"), all = TRUE )
components <- subset(components, 
                     select=-c(fixVersion, 
                               reporter, 
                               created, 
                               aggregatetimeoriginalestimate, 
                               
                               StartWeek, 
                               StartMonth, EndMonth, summary,
                               sprint, updated, priority, 
                               component.x,
                               type,  workDayToComplete,
                               workTimeToComplete, timeToComplete,
                                transitions, EndWeek))
components$originalEstimateDays <- components$timeoriginalestimate / 60 / 60 / 8
#add 1d for issues where there was no estimation
components$originalEstimateDays <-ifelse(is.na(components$originalEstimateDays), 1, 
                                         components$originalEstimateDays)

names(components)[names(components) == 'component.y'] <- 'component'
#View(components)
#stop()
#colnames(components )[9] <- "component"

components <- components[!is.na(components$movedToComplete), ]
components <- components[components$status != 'Rejected', ]
components <- components[order(components$movedToComplete), ]
components <- components[!is.na(components$component),]
components <- components[components$devOwner != '',]

#View(components)
#stop()

componentsAggr <- aggregate( x=components$originalEstimateDays,  by=list(components$project, 
                                                             components$component,
                                                             components$devOwner), FUN = sum)
colnames(componentsAggr )[1] <- "project"
colnames(componentsAggr )[2] <- "component"
colnames(componentsAggr )[3] <- "dev"
colnames(componentsAggr )[4] <- "count"


componentsAggr <- merge(componentsList, componentsAggr, by=c("project", "component"), all = TRUE )
#gg <- ggplot(componentsAggr, aes(x=hour, y=wkday, fill=n))

#View(componentsAggr)
#stop()

#-----------------------------------------------------------------------------------------------------------------------
#get reviewers data for issues & components
githubJiraMpping <- read.csv(
  file= './JiraGithubMapping.csv',
  head=TRUE,sep=";", dec=".", stringsAsFactors=FALSE)
#View(githubJiraMpping)
reviewers <- merge(reviewers, githubJiraMpping, by.x = 'reviewer', by.y = 'Github', all = TRUE )
#reviewers <- reviewers[reviewers$approver != 'False',]
reviewers$project = substr(reviewers$key, 1, 3)
reviewers <- merge(reviewers, componentIssue, by=c("key"), all.y  = FALSE )
reviewers$Jira <- ifelse(is.na(reviewers$Jira), reviewers$reviewer, reviewers$Jira)

#View(reviewers)
#stop()

reviewers <- subset(reviewers, 
                    select=-c(approver, reviewer, key))
reviewers <- reviewers[!is.na(reviewers$component),]
reviewers$count <- 1/8 #review counts as 1h of work on the issue, 1/8d = 1h
names(reviewers)[names(reviewers) == 'Jira'] <- 'dev'


componentsAggr <- rbind(reviewers, componentsAggr)

componentsAggr <- aggregate( x=componentsAggr$count,  by=list(componentsAggr$project, 
                                                              componentsAggr$component,
                                                              componentsAggr$dev), FUN = sum)
colnames(componentsAggr )[1] <- "project"
colnames(componentsAggr )[2] <- "component"
colnames(componentsAggr )[3] <- "dev"
colnames(componentsAggr )[4] <- "count"

componentsAggr <- componentsAggr[componentsAggr$count >= 1,]
#hist(componentsAggr$count)
#View(componentsAggr)
#stop()


#---------------------------------------------------------------------------------------------------------
#display components heatmap
projects <- c('VEL', 'VIN', 'VBS')
for(currentProject in projects){
  projectComponents <- componentsAggr[componentsAggr$project == currentProject,]
  
  gg <- ggplot(projectComponents, aes(x=projectComponents$component, y=projectComponents$dev, fill=projectComponents$count))
  gg <- gg + geom_tile(color="white", size=0.1)
  gg <- gg + scale_fill_viridis(name="# Events")
  gg <- gg + coord_equal()
  gg <- gg + labs(x=NULL, y=NULL, title=paste("Stories delivered per component in ", currentProject))
  gg <- gg + theme_tufte(base_family="Helvetica")
  gg <- gg + theme(plot.title=element_text(hjust=0))
  gg <- gg + theme(axis.ticks=element_blank())
  gg <- gg + theme(axis.text=element_text(size=8))
  gg <- gg + theme(legend.title=element_text(size=8))
  gg <- gg + theme(legend.text=element_text(size=6))
  gg <- gg + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  print(gg)
}
#stop()
  

#-----------------------------------------------------------------------------------------------------------------------
# tempFinishedBugs <- finishedIssues[finishedIssues$type == 'Story',]
# tempFinishedBugs <- tempFinishedBugs[tempFinishedBugs$devOwner == '',]
# tempFinishedBugs <- tempFinishedBugs[tempFinishedBugs$movedToComplete > as.POSIXct('2016-07-01'),]
# tempFinishedBugs <- tempFinishedBugs[!tempFinishedBugs$status %in% c('Rejected', 'Frozen'),]
# View(tempFinishedBugs)
# stop()

#test <- finishedIssues[finishedIssues$key == 'VEL-1157',]
#View(test)
#stop()

#completed issues per week
completedIssuesPerWeek <- aggregate( x=finishedIssues$Quantity,  by=list(finishedIssues$EndWeek, finishedIssues$project), 
                                         FUN = sum)
colnames(completedIssuesPerWeek )[1] <- "week"
colnames(completedIssuesPerWeek )[2] <- "project"
colnames(completedIssuesPerWeek )[3] <- "count"

timeToCompleteWeeklyAggrMean <- aggregate( x=finishedIssues$timeToComplete,  by=list(finishedIssues$EndWeek, 
                                                                     finishedIssues$project), 
                                   FUN = mean)
colnames(timeToCompleteWeeklyAggrMean )[1] <- "week"
colnames(timeToCompleteWeeklyAggrMean )[2] <- "project"
colnames(timeToCompleteWeeklyAggrMean )[3] <- "meanTimeToFinish"

#View(timeToCompleteWeeklyAggrMean)

timeToCompleteWeeklyAggrSD <- aggregate( x=finishedIssues$timeToComplete,  by=list(finishedIssues$EndWeek, 
                                                                                     finishedIssues$project), 
                                           FUN = sd)
colnames(timeToCompleteWeeklyAggrSD )[1] <- "week"
colnames(timeToCompleteWeeklyAggrSD )[2] <- "project"
colnames(timeToCompleteWeeklyAggrSD )[3] <- "sdTimeToFinish"

#View(timeToCompleteWeeklyAggrSD)
timeToCompleteWeeklyAggr <- merge(timeToCompleteWeeklyAggrMean, timeToCompleteWeeklyAggrSD, by=c("week", "project"), all = TRUE )
timeToCompleteWeeklyAggr <- merge(timeToCompleteWeeklyAggr, completedIssuesPerWeek, by=c("week", "project"), all = TRUE )

#View(timeToCompleteWeeklyAggr)

projects <- c('VEL', 'VBS', 'VIN')

for(currentProject in projects){
  timeToCompleteWeeklyAggrPerProject <- timeToCompleteWeeklyAggr[timeToCompleteWeeklyAggr$project == currentProject, ]
  
  plot <- ggplot(timeToCompleteWeeklyAggrPerProject, aes(timeToCompleteWeeklyAggrPerProject$week)) + 
    geom_point(aes(y=timeToCompleteWeeklyAggrPerProject$meanTimeToFinish)) +
    geom_line(aes(y=timeToCompleteWeeklyAggrPerProject$meanTimeToFinish, colour ="Mean time to finish")) +  # first layer
    geom_errorbar(aes(x=timeToCompleteWeeklyAggrPerProject$week, ymin=timeToCompleteWeeklyAggrPerProject$meanTimeToFinish - timeToCompleteWeeklyAggrPerProject$sdTimeToFinish, 
                      ymax=timeToCompleteWeeklyAggrPerProject$meanTimeToFinish + timeToCompleteWeeklyAggrPerProject$sdTimeToFinish), width=0.25) +
    ylim(0, 500) + xlab("week")+
    ylab("Mean time to finish [h]") +
    ggtitle(paste("Mean time to finish + standard deviation in project", currentProject, sep = " ")) +
    geom_hline(yintercept=336, linetype="dashed", color = "red") +
    geom_hline(yintercept=168, linetype="dashed", color = "3") +
    labs(fill = "")
    
  #print(plot)
}
#View(timeToCompleteWeeklyAggrPerProject)
#stop()
#-----------------------------------------------------------------------------------------------------------------------
#number of issues per week that exceeds 1W and 2W time to complete 


#View(finishedIssuesAggrTimeToComplete)
finishedIssues$timeToCompleteQuantified <-  ifelse(finishedIssues$timeToComplete < 168, "LT1W", 
                                                   ifelse(finishedIssues$timeToComplete >= 168 & finishedIssues$timeToComplete < 336, "GT1W", "GT2W"))
sprintStart <- as.Date('2016-08-01', "%Y-%m-%d")
tooLongIssues <- finishedIssues[finishedIssues$timeToCompleteQuantified %in% c('GT2W','GT1W'),]
tooLongIssues <- tooLongIssues[tooLongIssues$movedToComplete >= as.POSIXct("2016-8-1", format="%Y-%m-%d"),]
tooLongIssues <- tooLongIssues[tooLongIssues$project == 'VIN',]
tooLongIssues$estimation <- tooLongIssues$timeoriginalestimate / 60 / 60 / 8
tooLongIssues <- subset(tooLongIssues, select=-c(fixVersion, reporter, movedToComplete, created, aggregatetimeoriginalestimate, timeoriginalestimate,
                                                 Quantity, StartWeek, StartMonth, EndMonth))
tooLongIssues$timeToComplete <- tooLongIssues$timeToComplete / 24
tooLongIssues$workTimeToComplete <- tooLongIssues$workTimeToComplete / 7.5
tooLongIssues <- tooLongIssues[order(tooLongIssues$EndWeek), ]  
#View(tooLongIssues)
#stop()

finishedIssuesAggrToC <- aggregate( x=finishedIssues$Quantity,  by=list(finishedIssues$EndWeek, finishedIssues$project, 
                                                                             finishedIssues$timeToCompleteQuantified), 
                                         FUN = sum)
colnames(finishedIssuesAggrToC )[1] <- "week"
colnames(finishedIssuesAggrToC )[2] <- "project"
colnames(finishedIssuesAggrToC )[3] <- "ToC"
colnames(finishedIssuesAggrToC )[4] <- "count"

#View(finishedIssuesAggrToC)
#stop()

for(currentProject in projects){
  
  finishedIssuesAggrToCProject <- finishedIssuesAggrToC[finishedIssuesAggrToC$project == currentProject, ]
  finishedIssuesAggrToCProject$ToC <- factor(finishedIssuesAggrToCProject$ToC, 
                                             c("LT1W","GT1W","GT2W"))
  #View(finishedIssuesAggrToCProject)
  #stop()
  
  plot <- ggplot(data = finishedIssuesAggrToCProject, aes(x = finishedIssuesAggrToCProject$week, y = finishedIssuesAggrToCProject$count, 
                                                           fill = finishedIssuesAggrToCProject$ToC)) + 
    geom_bar(stat="identity") +
    ggtitle("Issues reported by source weekly") + scale_fill_manual( 
      values=c("green", "orange", "red") ) +
    ylab("Number of issues") +
    xlab("week") + ggtitle(paste("Completed issues weekly in project", currentProject, sep = " ")) +
    labs(fill = "Type")
  
  print(plot)
  #stop()
}


#-----------------------------------------------------------------------------------------------------------------------
#completed issues per week
completedIssuesAggrSumWeek <- aggregate( x=finishedIssues$Quantity,  by=list(finishedIssues$EndWeek, finishedIssues$project, 
                                                                         finishedIssues$type), 
                                     FUN = sum)
colnames(completedIssuesAggrSumWeek )[1] <- "week"
colnames(completedIssuesAggrSumWeek )[2] <- "project"
colnames(completedIssuesAggrSumWeek )[3] <- "type"
colnames(completedIssuesAggrSumWeek )[4] <- "count"



for(currentProject in projects){
  
  completedIssuesAggrSumProject <- completedIssuesAggrSumWeek[completedIssuesAggrSumWeek$project == currentProject, ]
  
  #completedIssuesAggrSumWeekNoType <- aggregate( x=completedIssuesAggrSumProject$count,  by=list(completedIssuesAggrSumProject$week), FUN = sum)
  #colnames(completedIssuesAggrSumWeekNoType )[1] <- "week"
  #colnames(completedIssuesAggrSumWeekNoType )[2] <- "count"
  #calculating average sprint velocity, how many issues team is delivering per sprint
  #completedIssuesAggrSumWeekNoType$rollAvgx2 <- rollmean(completedIssuesAggrSumWeekNoType$count, k = 2, fill = NA, align = "right")*2
  
  
  #View(completedIssuesAggrSumWeekNoType)
  #stop()
                                                   #
                                                 
  
  plot <- ggplot(data = completedIssuesAggrSumProject, aes(x = completedIssuesAggrSumProject$week, y = completedIssuesAggrSumProject$count, 
                                                           fill = completedIssuesAggrSumProject$type)) + 
    geom_bar(stat="identity") + 
    ggtitle("Issues reported by source weekly") + scale_fill_manual( values = c25 ) +
    ylab("Number of issues") +
    xlab("week") + ggtitle(paste("Completed issues weekly in project", currentProject, sep = " ")) +
    labs(fill = "Type")
    #geom_line(aes(x=completedIssuesAggrSumWeekNoType$week, y=completedIssuesAggrSumWeekNoType$count))  # second layer

  print(plot)
}
#stop()


#----------------------------------------------------------------------------------------------------------------------
#rolling average per 2 weeks
# for(currentProject in projects){
#   
#   completedIssuesAggrSumProject <- completedIssuesAggrSumWeek[completedIssuesAggrSumWeek$project == currentProject, ]
#   
#   completedIssuesAggrSumWeekNoType <- aggregate( x=completedIssuesAggrSumProject$count,  by=list(completedIssuesAggrSumProject$week), FUN = sum)
#   colnames(completedIssuesAggrSumWeekNoType )[1] <- "week"
#   colnames(completedIssuesAggrSumWeekNoType )[2] <- "count"
#   #calculating average sprint velocity, how many issues team is delivering per sprint
#   completedIssuesAggrSumWeekNoType$roll2WSum <- rollmean(completedIssuesAggrSumWeekNoType$count, k = 2, fill = NA, align = "right")
#   
#   plot <- ggplot(completedIssuesAggrSumWeekNoType, aes(completedIssuesAggrSumWeekNoType$week)) +                     # basic graphical object
#     geom_line(aes(y=completedIssuesAggrSumWeekNoType$count, colour ="2W sum")) +  # first layer
#     geom_point(aes(y=completedIssuesAggrSumWeekNoType$count, colour ="2W sum")) +
#     scale_colour_manual("Lines", values=c("2W sum"="blue")) + 
#     ggtitle(paste("2W sum of delivered issues in team:", 
#                   currentProject, sep = " ")) +
#     xlab("week")+
#     ylab("Number of issues")
#   #+
#   #geom_vline(xintercept=as.numeric(releasedVersions$releasedDateConverted), linetype="dashed") +
#   #geom_text(data=releasedVersions, mapping=aes(x=releasedVersions$releasedDateConverted, y=0, label=releasedVersions$name), size=4, angle=90, vjust=-0.4, hjust=0)
#   
#   print(plot)
#   
# }

#completed issues per month
#-----------------------------------------------------------------------------------------------------------------------
#completed issues per week

completedIssuesAggrSumMonth <- aggregate( x=finishedIssues$Quantity,  by=list(finishedIssues$EndMonth, finishedIssues$project, 
                                                                         finishedIssues$type), 
                                     FUN = sum)
colnames(completedIssuesAggrSumMonth )[1] <- "month"
colnames(completedIssuesAggrSumMonth )[2] <- "project"
colnames(completedIssuesAggrSumMonth )[3] <- "type"
colnames(completedIssuesAggrSumMonth )[4] <- "count"

for(currentProject in projects){
  
  completedIssuesAggrSumMonthProject <- completedIssuesAggrSumMonth[completedIssuesAggrSumMonth$project == currentProject, ]
  
  plot <- ggplot(data = completedIssuesAggrSumMonthProject, aes(x = completedIssuesAggrSumMonthProject$month,
                                                                y = completedIssuesAggrSumMonthProject$count,
                                                           fill = completedIssuesAggrSumMonthProject$type)) +
    geom_bar(stat="identity") +
    ggtitle("Issues reported by source weekly") + scale_fill_manual( values = c25 ) +
    ylab("Number of issues") +
    xlab("month") + ggtitle(paste("Completed issues monthly in project", currentProject, sep = " ")) +
    labs(fill = "Type")

  print(plot)
  #View(completedIssuesAggrSumMonthProject)
  #stop()
}
#-----------------------------------------------------------------------------------------------------------------------
#bugs created vs solved + number of not solved bugs per team weekly monthly
#completed issues per week

bugs <- issues[issues$type == "Bug",]
#velBugs <- bugs[bugs$project == 'VEL',]
#View(velBugs)
#stop()
createdIssuesByWeek <- aggregate( x=bugs$Quantity,  by=list(bugs$StartWeek, bugs$project), 
                                  FUN = sum)

colnames(createdIssuesByWeek )[1] <- "week"
colnames(createdIssuesByWeek )[2] <- "project"
colnames(createdIssuesByWeek )[3] <- "opened"
#View(createdIssuesByWeek)
#stop()

#bugsTest <- bugs[is.na( bugs$movedToComplete), ]
#bugsTest <- bugsTest[bugsTest$project == 'VEL',]
#View(bugsTest)
#stop()

allWeeks <- data.frame(week = seq(from = min(createdIssuesByWeek$week), 
                                  to = max(createdIssuesByWeek$week), by="+1 week"))
#View(allWeeks)

colnames(allWeeks)[1] <- "week"
allWeeks[, "opened"] <- 0
allWeeks[, "completed"] <- 0
allWeeks[, "left"] <- 0


finishedBugs <- finishedIssues[finishedIssues$type == 'Bug', ]
#View(finishedBugs)
#finishedBugsVel <- finishedBugs[finishedBugs$project == 'VEL',]
#finishedBugsVel <- finishedBugsVel[finishedBugsVel$movedToComplete >= as.POSIXct('2016-08-01'),]
#View(finishedBugsVel)
#stop()

completedBugsByWeek <- aggregate( x=finishedBugs$Quantity,  by=list(finishedBugs$EndWeek, finishedBugs$project), 
                                         FUN = sum)
colnames(completedBugsByWeek )[1] <- "week"
colnames(completedBugsByWeek )[2] <- "project"
colnames(completedBugsByWeek )[3] <- "completed"
#View(completedIssuesAggrSumMonth)

#createdBugsTmp <- bugs[bugs$StartWeek == '2015-03-30' | bugs$StartWeek == '2015-03-23' , ]
#View(createdBugsTmp)
#finishedBugsTmp <- finishedBugs[finishedBugs$EndWeek == '2015-03-30' | finishedBugs$EndWeek == '2015-03-23', ]
#View(finishedBugsTmp)
#stop()
#createdVsClosedByWeekTest <- merge(finishedBugs, completedBugsByWeek, by=c("key"), all = TRUE)
#View(createdVsClosedByWeekTest)
#bugsTest <- finishedBugs[duplicated(finishedBugs$key),]
#View(bugsTest)
#stop()


createdVsClosedByWeek <- merge(createdIssuesByWeek, completedBugsByWeek, by=c("week", "project"), all = TRUE)

#remove NA values
createdVsClosedByWeek$opened[is.na(createdVsClosedByWeek$opened)] <- 0
createdVsClosedByWeek$completed[is.na(createdVsClosedByWeek$completed)] <- 0
#createdVsClosedByWeek$left[is.na(createdVsClosedByWeek$left)] <- 0

createdVsClosedByWeek[, "left"] <- as.integer(createdVsClosedByWeek$opened) - as.integer(createdVsClosedByWeek$completed)

#View(createdVsClosedByWeek)
#stop()

openBugs <- openIssues[openIssues$type == 'Bug',]
openBugs <- openBugs[openBugs$status == 'Backlog',]
openBugsAggr <- aggregate( x=openBugs$Quantity,  by=list(openBugs$project, openBugs$priority), 
                                  FUN = sum)


colnames(openBugsAggr )[1] <- "project"
colnames(openBugsAggr )[2] <- "priority"
colnames(openBugsAggr )[3] <- "open"
openBugsAggr$label <- paste(openBugsAggr$priority, openBugsAggr$open, sep = ": ")
#View(openBugsAggr)


openBugsPerComponent <- aggregate( x=openBugs$Quantity,  by=list(openBugs$project, openBugs$component), 
                           FUN = sum)


colnames(openBugsPerComponent )[1] <- "project"
colnames(openBugsPerComponent )[2] <- "component"
colnames(openBugsPerComponent )[3] <- "open"
openBugsPerComponent$label <- paste(openBugsPerComponent$component, openBugsPerComponent$open, sep = ": ")

for(currentProject in projects){
  tempAllWeeks <- allWeeks
  tempAllWeeks[, "project"] <- currentProject
  
  createdVsClosedOneProject <- createdVsClosedByWeek[createdVsClosedByWeek$project == currentProject, ]
  #View(createdVsClosedOneProject)
  #stop()
  
  missingWeeks <- tempAllWeeks[!(tempAllWeeks$week %in% (createdVsClosedOneProject$week)),]
  #View(missingWeeks)
  
  createdVsClosedOneProject <- rbind(createdVsClosedOneProject, missingWeeks) 
  createdVsClosedOneProject <- createdVsClosedOneProject[order(createdVsClosedOneProject$week), ]
  
  #createdVsClosedOneProject <- rbind(createdVsClosedOneProject, missingWeeks) 
  createdVsClosedOneProject <- createdVsClosedOneProject[order(createdVsClosedOneProject$week), ]
  createdVsClosedOneProject[, "cumSumLeft"] <- cumsum(createdVsClosedOneProject$left)
  
  #View(createdVsClosedOneProject)
  #stop()
  
  plot <- ggplot(createdVsClosedOneProject, aes(createdVsClosedOneProject$week)) +                     # basic graphical object
    geom_bar(stat="identity", aes(x = createdVsClosedOneProject$week, y = createdVsClosedOneProject$cumSumLeft), fill="white", colour="black")+
    geom_line(aes(y=createdVsClosedOneProject$opened, colour ="reported bugs")) +  # first layer
    geom_point(aes(y=createdVsClosedOneProject$opened, colour ="reported bugs")) +
    geom_line(aes(y=createdVsClosedOneProject$completed, colour ="closed bugs"))+  # second layer
    geom_point(aes(y=createdVsClosedOneProject$completed, colour ="closed bugs"))+
    scale_colour_manual("Lines", values=c("closed bugs"="green", "reported bugs"="red")) + 
    ggtitle(paste("Bugs reported vs closed (weekly) + cumulative sum of not closed bugs in team:", 
                  currentProject, sep = " ")) +
    xlab("week")+
    ylab("Number of bugs")
    #+
    #geom_vline(xintercept=as.numeric(releasedVersions$releasedDateConverted), linetype="dashed") +
    #geom_text(data=releasedVersions, mapping=aes(x=releasedVersions$releasedDateConverted, y=0, label=releasedVersions$name), size=4, angle=90, vjust=-0.4, hjust=0)
  
  print(plot)
  #View(createdVsClosedOneProject)
  #stop()
  #--------------------------------
  openBugsAggrCurrentProject <- openBugsAggr[openBugsAggr$project == currentProject, ]
  if(nrow(openBugsAggrCurrentProject) == 0) next
  
  
  plot <- pie(openBugsAggrCurrentProject$open, labels = openBugsAggrCurrentProject$label, col = c("blue", "red", "orange", "yellow", "pink", "grey"),
      main=paste("Open bugs in project: ", currentProject))
    print(plot)
 #--------------------------------   
    openBugsPerComponentCurrentProject <- openBugsPerComponent[openBugsPerComponent$project == currentProject, ]
    if(nrow(openBugsPerComponentCurrentProject) == 0) next
    plot <- pie(openBugsPerComponentCurrentProject$open, 
                labels = openBugsPerComponentCurrentProject$label, 
                col = c("blue", "red", "orange", "yellow", "pink", "grey"),
                main=paste("Open bugs in project: ", currentProject))
    print(plot)
}

#---------------------------------------------------------------------------------------------------------------
# estimation vs time to complete
estimatedFinishedIssues <- finishedIssues[!is.na(finishedIssues$timeoriginalestimate) | !is.na(finishedIssues$aggregatetimeoriginalestimate), ]
estimatedFinishedIssues <- subset(estimatedFinishedIssues, select=-c(sprint, reporter, 
                                                                     created, fixVersion, devOwner, StartMonth, EndMonth, Quantity))
#also remove 0 estimates
estimatedFinishedIssues <- estimatedFinishedIssues[estimatedFinishedIssues$timeoriginalestimate > 0 | estimatedFinishedIssues$aggregatetimeoriginalestimate > 0, ]

#time to complete is measured with 24h days. before we start comparing it to estimations it need to be normalized to
#7.5h day
# x / 24 * 7 = 7,5 / 5 * 7,5
#x = 33,6
estimatedFinishedIssues$timeToCompleteNormalized <- round(estimatedFinishedIssues$timeToComplete / 33.6, digits = 2)
#remove weekends and multiply rest of days by 7.5
#calculate how many weekends hours were in time to complete - 168 = 24 * 7
estimatedFinishedIssues$timeToCompleteWeekendHours <- (estimatedFinishedIssues$timeToComplete %/% 168) * 48
estimatedFinishedIssues$timeToCompleteNoWeekends <- estimatedFinishedIssues$timeToComplete - estimatedFinishedIssues$timeToCompleteWeekendHours
estimatedFinishedIssues$timeToCompleteNormalizedToManDay <- estimatedFinishedIssues$timeToCompleteNoWeekends / 24 * 7.5

#seconds to hours
estimatedFinishedIssues$timeoriginalestimate <- round(estimatedFinishedIssues$timeoriginalestimate / 60 / 60, digits = 3)
estimatedFinishedIssues$aggregatetimeoriginalestimate <- round(estimatedFinishedIssues$aggregatetimeoriginalestimate / 60 / 60, digits = 3)

estimatedFinishedIssues$estimateDifference <- estimatedFinishedIssues$timeToCompleteNormalizedToManDay - estimatedFinishedIssues$timeoriginalestimate
estimatedFinishedIssues$estimateDifferencePerc <- round(estimatedFinishedIssues$estimateDifference / estimatedFinishedIssues$timeoriginalestimate * 100, digits = 2)

estimatedFinishedIssuesAggr <- aggregate( x=estimatedFinishedIssues$estimateDifference,  
                                          by=list(estimatedFinishedIssues$EndWeek, estimatedFinishedIssues$project), FUN = mean)
colnames(estimatedFinishedIssuesAggr )[1] <- "week"
colnames(estimatedFinishedIssuesAggr )[2] <- "project"
colnames(estimatedFinishedIssuesAggr )[3] <- "meanEstimationDifference"

estimatedFinishedIssuesAggrPerc <- aggregate( x=estimatedFinishedIssues$estimateDifferencePerc,  
                                          by=list(estimatedFinishedIssues$EndWeek, estimatedFinishedIssues$project), FUN = mean)
colnames(estimatedFinishedIssuesAggrPerc )[1] <- "week"
colnames(estimatedFinishedIssuesAggrPerc )[2] <- "project"
colnames(estimatedFinishedIssuesAggrPerc )[3] <- "meanEstimationDifferencePerc"
#View(estimatedFinishedIssuesAggr)

#----------------------------------------------------------------------------------
#comments in velocity core per person
# gitHubCommentsAggrWeek <- aggregate( x=gitHubComments$Quantity,  by=list(gitHubComments$createdWeek, gitHubComments$author), 
#                                           FUN = sum)
# colnames(gitHubCommentsAggrWeek )[1] <- "week"
# colnames(gitHubCommentsAggrWeek )[2] <- "author"
# colnames(gitHubCommentsAggrWeek )[3] <- "count"
# 
# notInCoreTeam <- c("AgnesAnn", "KarolBialyKainos", "arompa", 'artursenk',
#                    'KrzysztofKowalskiKainos', 'grzegorz-borczuch', 'michalschott',
#                    'krzysztofsk', 'marines', 'CommanderK5', 'grzegorz-gn',
#                    'szymonrkainos', 'piotrs-kainos', 'robchare')
# 
# gitHubCommentsAggrWeek <- gitHubCommentsAggrWeek[!(gitHubCommentsAggrWeek$author %in% notInCoreTeam),]
# 
# #View(gitHubCommentsAggrWeek)
# 
#   
#  ggplot(data = gitHubCommentsAggrWeek, aes(x = gitHubCommentsAggrWeek$week,
#                                                                 y = gitHubCommentsAggrWeek$count,
#                                                                 fill = gitHubCommentsAggrWeek$author)) +
#     geom_bar(stat="identity") +
#     ggtitle("Core GitHub comments weekly") + scale_fill_manual( values = c25 ) +
#     ylab("Comments") +
#     xlab("week") +
#     labs(fill = "author") +
#     geom_vline(xintercept=as.numeric(as.Date('2016-05-01', format="%Y-%m-%d")), linetype="dotted") +
#     geom_vline(xintercept=as.numeric(as.Date('2016-07-01', format="%Y-%m-%d")), linetype="dotted")
#    
   #geom_text(data=releasedVersions, mapping=aes(x=releasedVersions$releasedDateConverted, y=0, label=releasedVersions$name), size=4, angle=90, vjust=-0.4, hjust=0)


#----------------------------------------------------------------------------------------------------------------
# completed vs not completed issues per sprint
 #---------------------------------------------------------------------------------------------------------------
 #Theses graphics have the throughput on the horizontal (x) axis and the count frequency on the vertical (y) axis. 
 #The benefit of this chart is that it gives us an overall idea of the shape of the distribution of your underlying data.
 #completed issues per week
 finishedIssues3M <- finishedIssues[finishedIssues$movedToComplete >= as.POSIXct.Date( minus3M), ]
 completedIssuesAggrSumWeekNoType <- aggregate( x=finishedIssues3M$Quantity,  by=list(finishedIssues3M$EndWeek, finishedIssues3M$project), 
                                          FUN = sum)
 colnames(completedIssuesAggrSumWeekNoType )[1] <- "week"
 colnames(completedIssuesAggrSumWeekNoType )[2] <- "project"
 colnames(completedIssuesAggrSumWeekNoType )[3] <- "count"
 #View(completedIssuesAggrSumWeekNoType)
 #stop()
 
 
 
 for(currentProject in projects){
   
   completedIssuesAggrSumWeekNoTypePerProject <- completedIssuesAggrSumWeekNoType[completedIssuesAggrSumWeekNoType$project == currentProject, ]
   plot <- hist(completedIssuesAggrSumWeekNoTypePerProject$count, main = paste("Histogram of issues completed weekly (last 3M) for", currentProject),
                breaks = 50, xlab = "Number of issues delivered per week last 3M", labels = FALSE, freq = TRUE)
   
   medianTime <- median(completedIssuesAggrSumWeekNoTypePerProject$count)
   abline(v = medianTime , col = "royalblue", lwd = 2)
   plotLbl <- paste("Median delivered per week: ", round(medianTime, 2), sep = " ")
   text(medianTime, 1.5, plotLbl, col = "royalblue", adj = c(-.1, -.1))
   
   #plot <- plot + annotate(round(meanTime), x = meanTime, y = 1)
   print(plot)
 }
 

 finishedBugs3M <- finishedBugs[finishedBugs$movedToComplete >= as.POSIXct.Date(minus3M) ,]
 #remove not representative bugs
 bugsToRemove <- c('VEL-300', 'VEL-396', 'VEL-398')
 finishedBugs3M <- finishedBugs3M[!(finishedBugs3M$key %in% bugsToRemove),]
 
 for(currentProject in projects){
   
   
   finishedBugsPerProject <- finishedBugs3M[finishedBugs3M$project == currentProject, ]
   if(nrow(finishedBugsPerProject) == 0) next;
  
   
   plot <- hist(finishedBugsPerProject$workDayToComplete, main = paste("Last 3M bugs time to complete", currentProject),
                breaks = 50, xlab = "Time to complete", labels = FALSE, freq = TRUE)
   bugMedian <- median(finishedBugsPerProject$workDayToComplete)
   
   abline(v = bugMedian, col = "royalblue", lwd = 2)
   
   plotLbl <- paste("Time to fix median: ", round(bugMedian, 2), sep = " ")
   
   text(bugMedian, 1.5, plotLbl, col = "royalblue", adj = c(-.1, -.1))
   
   print(plot)
   
   #View(finishedBugsPerProject)
   #stop()
 }
 
 #----------------------------------------------------------------------------------------------
 #issues completed per user per component
 finishedIssuesInProjectUserComponent <- list()
 finishedIssuesDevOwnerComponent <- finishedIssues[finishedIssues$devOwner != '',]
 finishedIssuesDevOwnerComponent <- finishedIssuesDevOwnerComponent[finishedIssuesDevOwnerComponent$component != '',]
 #finishedIssuesDevOwnerComponent <- finishedIssuesDevOwnerComponent[!( is.na(finishedIssuesDevOwnerComponent$component) 
 #                                                                         || finishedIssuesDevOwnerComponent$component == ''),]
 #View(finishedIssuesDevOwnerComponent)
 #stop()
 
 for(currentProject in projects)
 {
   finishedIssuesInProject <- finishedIssuesDevOwnerComponent[finishedIssuesDevOwnerComponent$project == currentProject,]
   
    currentFinishedIssuesInProjectUserComponent <- aggregate( x=finishedIssuesInProject$Quantity,  
                                                                        by=list(finishedIssuesInProject$devOwner, finishedIssuesInProject$component), 
                                                         FUN = sum)
   
   colnames(currentFinishedIssuesInProjectUserComponent )[1] <- "devOwner"
   colnames(currentFinishedIssuesInProjectUserComponent )[2] <- "component"
   colnames(currentFinishedIssuesInProjectUserComponent )[3] <- "count"
   
   finishedIssuesInProjectUserComponent[[currentProject]] <- currentFinishedIssuesInProjectUserComponent
 }
 
# test <- dcast(finishedIssuesInProjectUserComponent[['VEL']],  devOwner ~ component)
 #test <- reshape(finishedIssuesInProjectUserComponent[[currentProject]], idvar = "devOwner", timevar = "component", direction = "wide")
# View(test)
 
 #View(finishedIssuesInProjectUserComponent[['VEL']])
 
 finishedIssuesMovedBack <- finishedIssues[(finishedIssues$codeReviewToDev > 0) | (finishedIssues$testsToDev > 0),]
 finishedIssuesMovedBackVBS <- finishedIssuesMovedBack[finishedIssuesMovedBack$project == 'VBS',]
 finishedIssuesMovedBackVBS <- finishedIssuesMovedBackVBS[order(finishedIssuesMovedBackVBS$EndWeek), ]
 finishedIssuesMovedBackVBS$returnsSum <- finishedIssuesMovedBackVBS$codeReviewToDev + finishedIssuesMovedBackVBS$testsToDev
 finishedIssuesMovedBackVBS <- finishedIssuesMovedBackVBS[finishedIssuesMovedBackVBS$returnsSum >= 4,]
 
 finishedIssuesMovedBackVBS <- subset(finishedIssuesMovedBackVBS, select = c(key, summary, EndWeek, codeReviewToDev,
                                                                             testsToDev, returnsSum, devOwner))
 #View(finishedIssuesMovedBackVBS)
 #stop()
 
 finishedIssuesMovedBackAggrWeek <- aggregate( x=cbind(finishedIssuesMovedBack$codeReviewToDev,
                                                       finishedIssuesMovedBack$testsToDev),  
                                               by=list(finishedIssuesMovedBack$EndWeek,
                                                       finishedIssuesMovedBack$project), FUN = sum)

 colnames(finishedIssuesMovedBackAggrWeek )[1] <- "finishWeek"
 colnames(finishedIssuesMovedBackAggrWeek )[2] <- "project"
 colnames(finishedIssuesMovedBackAggrWeek )[3] <- "codeReviewToDev"
 colnames(finishedIssuesMovedBackAggrWeek )[4] <- "testsToDev"
 
 allWeeks <- data.frame(week = seq(from = min(finishedIssuesMovedBackAggrWeek$finishWeek), 
                                   to = max(finishedIssuesMovedBackAggrWeek$finishWeek), by="+1 week"))
 colnames(allWeeks)[1] <- "finishWeek"
 allWeeks[, "variable"] <- 'codeReviewToDev'
 allWeeks[, "value"] <- 0
 
 
 finishedIssuesMovedBackAggrWeek <- melt(finishedIssuesMovedBackAggrWeek, id=c("finishWeek","project")) 
 #View(finishedIssuesMovedBackAggrWeek)
 #stop()
 
 completedIssuesAggrSumWeek <- aggregate(by=list(completedIssuesAggrSumWeek$week,
                                                 completedIssuesAggrSumWeek$project), 
                                         x = completedIssuesAggrSumWeek$count, FUN = sum)
 colnames(completedIssuesAggrSumWeek)[1] <- 'finishWeek'
 colnames(completedIssuesAggrSumWeek)[2] <- 'project'
 colnames(completedIssuesAggrSumWeek)[3] <- 'count'
 #View(completedIssuesAggrSumWeek)
 #stop()
 
 
 for(currentProject in projects)
 {
   allWeeks$project <- currentProject
   
   finishedIssuesMovedBackAggrWeekPerProject <- finishedIssuesMovedBackAggrWeek[
     finishedIssuesMovedBackAggrWeek$project == currentProject,]
   
   missingWeeks <- allWeeks[!(allWeeks$finishWeek %in% (finishedIssuesMovedBackAggrWeekPerProject$finishWeek)),]
   finishedIssuesMovedBackAggrWeekPerProject <- rbind(finishedIssuesMovedBackAggrWeekPerProject, missingWeeks)
   finishedIssuesMovedBackAggrWeekPerProject <- merge(finishedIssuesMovedBackAggrWeekPerProject, completedIssuesAggrSumWeek,
                                                       by.y = c("finishWeek", "project"), by.x = c("finishWeek", "project"),
                                                      all.x = TRUE
                                                      )
   finishedIssuesMovedBackAggrWeekPerProject$transitionsNormalized <- 
      finishedIssuesMovedBackAggrWeekPerProject$value / finishedIssuesMovedBackAggrWeekPerProject$count

   #View(finishedIssuesMovedBackAggrWeekPerProject)
   #stop()

   plot <- ggplot(data = finishedIssuesMovedBackAggrWeekPerProject, 
                  aes(x = finishedIssuesMovedBackAggrWeekPerProject$finishWeek,
                      y = finishedIssuesMovedBackAggrWeekPerProject$transitionsNormalized,
                      fill = finishedIssuesMovedBackAggrWeekPerProject$variable)) +
                  geom_bar(stat="identity") +
     ggtitle(paste("Returns from tests & code review weekly in project:", currentProject)) + 
     scale_fill_manual( values = c("yellow", "orange") ) +
     ylab("Number of transitions / number of issues finished") +
     xlab("week") +
     labs(fill  = "Type")
   
   print(plot)
   
   plot <- ggplot(data = finishedIssuesMovedBackAggrWeekPerProject, 
                  aes(x = finishedIssuesMovedBackAggrWeekPerProject$finishWeek,
                      y = finishedIssuesMovedBackAggrWeekPerProject$value,
                      fill = finishedIssuesMovedBackAggrWeekPerProject$variable)) +
     geom_bar(stat="identity") +
     ggtitle(paste("Returns from tests & code review weekly in project:", currentProject)) + 
     scale_fill_manual( values = c("yellow", "orange") ) +
     ylab("Number of transitions") +
     xlab("week") +
     labs(fill  = "Type")
   
   print(plot)
 }