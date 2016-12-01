Sys.setlocale("LC_ALL","English")

library(xlsx)
library(scales)
library(ggplot2)
library(xtable)
library(lubridate)
library(zoo)

#uncomment for pdf
#pdf(file='C:\\Users\\robertk\\OneDrive for Business\\Documents\\dev process changes\\plot1.pdf', width=15, height=7)

par(mar=c(5,3,2,2)+0.1)

rm(list = ls())

#bugs <- read.csv(
#  file="C:\\Users\\robertk\\OneDrive for Business\\Documents\\dev process changes\\SmartBugsSinceNov20152015-11-23T10.26.01.9514632+01.00.csv",
#  head=TRUE,sep=",", dec=",", stringsAsFactors=FALSE)



json_file_versions <- "/Users/robertk/Office365/OneDrive - Kainos Software Ltd/Documents/VelocityReporting/data/SmartVersionsJson2016-05-24 12.07.07.734489.json"


#--------------------------------------------------------------------------------------------
#constant variables

#pallette
c25 <- c("dodgerblue2","#E31A1C", # red
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

#current date minus 3m
minus3M <- as.Date(Sys.Date()) %m+% months(-3)

ProfServ <- c("P.Sheridan"    ,                    
              "P.Makowska"    ,                    
              "w.murray"      ,                    
              "J.Flynn"       ,
              "a.dubowicz"    ,
              "j.martin"      ,
              "r.digby"       ,
              "S_Haggan"      ,    
              "Carol"         ,    
              "Marek"         ,    
              "a.mclaughlin"  ,      
              "C.Lennon"      ,      
              "C.Hughes"      ,      
              "S.Hanna"          )

reportStart = '2015-10-05'


#--------------------------------------------------------------------------------------
library("jsonlite")

versions <- stream_in(file(json_file_versions))
versions[, "nameLength"] <- nchar(versions$name)
versions[, "isMajor"] <- versions$nameLength <= 10
versions[, "releasedDateConverted"] <- as.Date(versions$releaseDate, format="%Y-%m-%d")
#View(versions)
#oldClosedReleasedIssues <- oldClosedReleasedIssues[oldClosedReleasedIssues$isReleased == "TRUE",]
# releasedMajorversions <- versions[versions$isMajor == "TRUE" ,]
# releasedMajorversions <- releasedMajorversions[releasedMajorversions$released == "TRUE", ]
# releasedMajorversions$releasedDateConverted

releasedVersions <- versions[versions$released == "TRUE", ]
releasedVersions <- releasedVersions[!is.na(releasedVersions$releasedDateConverted),]
releasedVersions <- releasedVersions[releasedVersions$releasedDateConverted >= as.Date(reportStart), ]


#View(releasedVersions)


#---------------------------------------------------------------------------------------

bugs[, "Source"] <- ifelse(bugs$reporter %in% ProfServ, "Client", "Smart")

bugs$updated <- as.POSIXct(bugs$updated, format="%Y-%m-%dT%H:%M:%S")

bugs[, "StartMonth"] <- as.Date(cut(as.Date(bugs$start), breaks = "month"))
bugs[, "EndMonth"] <- as.Date(cut(as.Date(bugs$end), breaks = "month")) 
bugs[, "StartWeek"] <- as.Date(cut(as.Date(bugs$start), breaks = "week"))
bugs[, "EndWeek"] <- as.Date(cut(as.Date(bugs$end), breaks = "week"))

# remove NA from isReleased
#bugs[is.na(bugs$isReleased)] <- 'False'
#if isReleased is empty - fixVersion not set -> not released
bugs$isReleased[bugs$isReleased == ""] <- "False"
#unique(bugs$isReleased)

#View(bugs)

#------------------------------------------------------------------------------
#only reported items

#createdOnly <- aggregate(x=as.Date(bugs$created), by=list(bugs$key), min)
createdOnly <- unique(bugs[,c("key","created")])
colnames(createdOnly)[1] <- "key"
colnames(createdOnly)[2] <- "created"
# View(createdOnly)
# stop()
createdOnly[, "CreatedWeek"] <- as.Date(cut(as.Date(createdOnly$created), breaks = "week"))
createdOnly[, "CreatedMonth"] <- as.Date(cut(as.Date(createdOnly$created), breaks = "month"))
createdOnly <- createdOnly[order(createdOnly$created), ]
createdOnly[, "Quantity"] <- 1


issueComponent <- unique(bugs[,c("key","component")])
issueType <- unique(bugs[,c("key","type")])
issueSource <- unique(bugs[,c("key","Source")])
issueCause <- unique(bugs[,c("key","cause")])
issueTeam <- unique(bugs[, c("key", "team")])


createdOnly <- merge(issueComponent, createdOnly, by="key")
createdOnly <- merge(issueType, createdOnly, by="key")
createdOnly <- merge(issueSource, createdOnly, by="key")
createdOnly <- merge(issueCause, createdOnly, by="key")
createdOnly <- merge(issueTeam, createdOnly, by="key")

createdOnly <- createdOnly[createdOnly$created >= reportStart,]

#View(createdOnly)

#-------------------------------------------------------------------------------------
# finished issues
finishedIssues <- bugs[bugs$status %in% c("Closed", "Testing completed"),]

isIDmax <- with(finishedIssues, ave(updated, key, FUN=function(x) .POSIXct(seq_along(x)==which.max(x)))==1)
finishedIssues <- finishedIssues[isIDmax, ]


#finishedIssues <- finishedIssues[finishedIssues$start >= reportStart,]

finishedIssues <- finishedIssues[order(finishedIssues$start), ]
#finishedIssues <- finishedIssues[finishedIssues$status == finishedIssues$ToStatus, ]
finishedIssues[, "Quantity"] <- 1
View(finishedIssues)

#find max updated value for each jira key


noTeamFinished <- finishedIssues[finishedIssues$team == "", ]
View(noTeamFinished)
#noTeamFinished$key
#stop()


completedByTeamAggr  <- aggregate(x=finishedIssues$Quantity, by=list(finishedIssues$StartWeek, 
                                                                     finishedIssues$team), sum)
colnames(completedByTeamAggr )[1] <- "week"
colnames(completedByTeamAggr )[2] <- "team"
colnames(completedByTeamAggr )[3] <- "closed"
#View(completedByTeamAggr )


finishedIssuesAggr <- aggregate(x=finishedIssues$Quantity, by=list(finishedIssues$StartWeek), sum)


colnames(finishedIssuesAggr)[1] <- "week"
colnames(finishedIssuesAggr)[2] <- "closed"

#View(finishedIssuesAggr)


createdIssuesByTeamAggr <- aggregate(x=createdOnly$Quantity, by=list(createdOnly$CreatedWeek, 
                                                                     createdOnly$team), sum)
colnames(createdIssuesByTeamAggr)[1] <- "week"
colnames(createdIssuesByTeamAggr)[2] <- "team"
colnames(createdIssuesByTeamAggr)[3] <- "opened"
#View(createdIssuesByTeamAggr)

createdIssuesAggr <- aggregate(x=createdOnly$Quantity, by=list(createdOnly$CreatedWeek), sum)
colnames(createdIssuesAggr)[1] <- "week"
colnames(createdIssuesAggr)[2] <- "opened"

createdVsClosed <- merge(createdIssuesAggr, finishedIssuesAggr, by=c("week") )
createdVsClosed[, "left"] <- as.integer(createdVsClosed$opened) - as.integer(createdVsClosed$closed)
createdVsClosed[, "cumSumLeft"] <- cumsum(createdVsClosed$left)
#View(createdVsClosed)

createdVsClosedbyTeam <- merge(createdIssuesByTeamAggr, completedByTeamAggr , by=c("week", "team"))
createdVsClosedbyTeam[, "left"] <- as.integer(createdVsClosedbyTeam$opened) - as.integer(createdVsClosedbyTeam$closed)
#View(createdVsClosedbyTeam)

teams <- unique(createdVsClosedbyTeam$team)

#--------------------------------------------------------------------------------------
#created per week cause

defectsReportedWeeklyByCause <- aggregate(x=createdOnly$Quantity, by=list(createdOnly$CreatedWeek, createdOnly$cause), sum)
#defectsReportedWeekly <- defectsReportedWeekly[order(defectsReportedWeekly$CreatedWeek),]

colnames(defectsReportedWeeklyByCause)[1] <- "week"
colnames(defectsReportedWeeklyByCause)[2] <- "cause"
colnames(defectsReportedWeeklyByCause)[3] <- "Reported"

#View(defectsReportedWeeklyByCause)
# #stop()


ggplot(data = defectsReportedWeeklyByCause, aes(x = defectsReportedWeeklyByCause$week, y = defectsReportedWeeklyByCause$Reported, 
                                                fill = defectsReportedWeeklyByCause$cause)) + 
  geom_bar(stat="identity") + 
  ggtitle("Issues reported by cause weekly") + scale_fill_manual( values = c25 )+
  ylab("Number of bugs") +
  xlab("week")

#stop()
#---------------------------------------------------------------------------------------
#created per week source

defectsReportedWeeklyBySource <- aggregate(x=createdOnly$Quantity, by=list(createdOnly$CreatedWeek, createdOnly$Source), sum)
#defectsReportedWeekly <- defectsReportedWeekly[order(defectsReportedWeekly$CreatedWeek),]

colnames(defectsReportedWeeklyBySource)[1] <- "week"
colnames(defectsReportedWeeklyBySource)[2] <- "Source"
colnames(defectsReportedWeeklyBySource)[3] <- "Reported"

#View(defectsReportedWeeklyByCause)
# #stop()

plot <- ggplot(data = defectsReportedWeeklyBySource, aes(x = defectsReportedWeeklyBySource$week, y = defectsReportedWeeklyBySource$Reported, 
                                                         fill = defectsReportedWeeklyBySource$Source)) + 
  geom_bar(stat="identity") + 
  ggtitle("Issues reported by source weekly") + scale_fill_manual( values = c25 )+
  ylab("Number of bugs") +
  xlab("week")
#   geom_vline(xintercept=as.numeric(releasedVersions$releasedDateConverted), linetype="dashed")
# 
# plot <- plot + geom_text(data=releasedVersions, 
#                           mapping=aes(x=releasedDateConverted, y=0, 
#                                       label=name), size=4, angle=90, vjust=-0.4, hjust=0)
plot
#---------------------------------------------------------------------------------------
#created per week type

defectsReportedWeeklyByType <- aggregate(x=createdOnly$Quantity, by=list(createdOnly$CreatedWeek, createdOnly$type), sum)
#defectsReportedWeekly <- defectsReportedWeekly[order(defectsReportedWeekly$CreatedWeek),]

colnames(defectsReportedWeeklyByType)[1] <- "week"
colnames(defectsReportedWeeklyByType)[2] <- "type"
colnames(defectsReportedWeeklyByType)[3] <- "Reported"

allWeeks <- data.frame(week = seq(from = min(defectsReportedWeeklyByType$week), 
                                  to = max(defectsReportedWeeklyByType$week), by="+1 week"))


allWeeks[, "Reported"] <- 0
allWeeks[, "type"] <- "Automated test sub-task"
colnames(allWeeks)[1] <- "week"

missingWeeks <- allWeeks[!(allWeeks$week %in% (
  defectsReportedWeeklyByType$week[defectsReportedWeeklyByType$type == "Automated test sub-task"])),]

defectsReportedWeeklyByType <- rbind(defectsReportedWeeklyByType, missingWeeks) 
defectsReportedWeeklyByType <- defectsReportedWeeklyByType[order(defectsReportedWeeklyByType$type), ]
#View(missingWeeks)
#View(defectsReportedWeeklyByType)



ggplot(data = defectsReportedWeeklyByType, aes(x = defectsReportedWeeklyByType$week, y = defectsReportedWeeklyByType$Reported, 
                                               fill = defectsReportedWeeklyByType$type)) + 
  geom_bar(stat="identity") + 
  ggtitle("Issues reported by type weekly") + scale_fill_manual( values = c25 )+
  ylab("Number of bugs") +
  xlab("week")


#----------------------------------------------------------------------------------------------------------
#created per week team

defectsReportedWeeklyByTeam <- aggregate(x=createdOnly$Quantity, by=list(createdOnly$CreatedWeek, createdOnly$team), sum)

#defectsReportedWeekly <- defectsReportedWeekly[order(defectsReportedWeekly$CreatedWeek),]

colnames(defectsReportedWeeklyByTeam)[1] <- "week"
colnames(defectsReportedWeeklyByTeam)[2] <- "team"
colnames(defectsReportedWeeklyByTeam)[3] <- "Reported"

#View(defectsReportedWeeklyByTeam)
# #stop()

#str(releasedVersions)
#library(reshape2)
#meltedReleaseVersions <- melt(releasedVersions, id.vars = c("name", "releasedDateConverted"))

ggplot(data = defectsReportedWeeklyByTeam, aes(x = defectsReportedWeeklyByTeam$week, y = defectsReportedWeeklyByTeam$Reported, 
                                               fill = defectsReportedWeeklyByTeam$team)) + 
  geom_bar(stat="identity") + 
  #geom_vline(xintercept=as.numeric(releasedVersions$releasedDateConverted), linetype="dashed")+
  ggtitle("Issues reported by team weekly") + scale_fill_manual( values = c25 ) +
  ylab("Number of bugs") +
  xlab("week")
#geom_text(data=releasedVersions, mapping=aes(x=releasedVersions$releasedDateConverted, y=0, label=releasedVersions$name), size=4, angle=90, vjust=-0.4, hjust=0)



#--------------------------------------------------------------------------------------
#created vs closed
ggplot(createdVsClosed, aes(createdVsClosed$week)) +                     # basic graphical object
  geom_bar(stat="identity", aes(x = createdVsClosed$week, y = createdVsClosed$cumSumLeft), fill="white", colour="red")+
  geom_line(aes(y=createdVsClosed$opened, colour ="reported bugs")) +  # first layer
  geom_point(aes(y=createdVsClosed$opened, colour ="reported bugs")) +
  geom_line(aes(y=createdVsClosed$closed, colour ="closed bugs"))+  # second layer
  geom_point(aes(y=createdVsClosed$closed, colour ="closed bugs"))+
  scale_colour_manual("Lines", values=c("closed bugs"="green", "reported bugs"="red"))+
  ggtitle("Bugs reported vs closed (weekly) + cumulative sum of not closed bugs (All teams)") +
  xlab("week")+
  ylab("Number of bugs") +
  geom_vline(xintercept=as.numeric(releasedVersions$releasedDateConverted), linetype="dashed") +
  geom_text(data=releasedVersions, mapping=aes(x=releasedVersions$releasedDateConverted, y=0, label=releasedVersions$name), size=4, angle=90, vjust=-0.4, hjust=0)

#-----------------------------------------------------------------------------------------

allWeeks <- data.frame(week = seq(from = min(defectsReportedWeeklyByType$week), 
                                  to = max(defectsReportedWeeklyByType$week), by="+1 week"))

colnames(allWeeks)[1] <- "week"
allWeeks[, "opened"] <- 0
allWeeks[, "closed"] <- 0
allWeeks[, "left"] <- 0
#View(allWeeks)

teams <- c("HCM Team", "Financials Team", "Features Team", "Security Team", "Core Team", "")
#teams <- c("HCM Team")

for(team in teams){
  #team <- "Features Team"
  
  #print(team)
  
  #createdVsClosed
  
  tempAllWeeks <- allWeeks
  tempAllWeeks[, "team"] <- team
  
  createdVsClosedOneTeam <- createdVsClosedbyTeam[createdVsClosedbyTeam$team == team, ]
  
  
  
  
  missingWeeks <- tempAllWeeks[!(tempAllWeeks$week %in% (
    createdVsClosedOneTeam$week)),]
  
  createdVsClosedOneTeam <- rbind(createdVsClosedOneTeam, missingWeeks) 
  createdVsClosedOneTeam <- createdVsClosedOneTeam[order(createdVsClosedOneTeam$week), ]
  createdVsClosedOneTeam[, "cumSumLeft"] <- cumsum(createdVsClosedOneTeam$left)
  
  #View(createdVsClosedOneTeam)
  
  plot <- ggplot(createdVsClosedOneTeam, aes(createdVsClosedOneTeam$week)) +                     # basic graphical object
    geom_bar(stat="identity", aes(x = createdVsClosedOneTeam$week, y = createdVsClosedOneTeam$cumSumLeft), fill="white", colour="red")+
    geom_line(aes(y=createdVsClosedOneTeam$opened, colour ="reported bugs")) +  # first layer
    geom_point(aes(y=createdVsClosedOneTeam$opened, colour ="reported bugs")) +
    geom_line(aes(y=createdVsClosedOneTeam$closed, colour ="closed bugs"))+  # second layer
    geom_point(aes(y=createdVsClosedOneTeam$closed, colour ="closed bugs"))+
    scale_colour_manual("Lines", values=c("closed bugs"="green", "reported bugs"="red")) + 
    ggtitle(paste("Bugs reported vs closed (weekly) + cumulative sum of not closed bugs in team:", 
                  team, sep = " ")) +
    xlab("week")+
    ylab("Number of bugs")+
    geom_vline(xintercept=as.numeric(releasedVersions$releasedDateConverted), linetype="dashed") +
    geom_text(data=releasedVersions, mapping=aes(x=releasedVersions$releasedDateConverted, y=0, label=releasedVersions$name), size=4, angle=90, vjust=-0.4, hjust=0)
  
  print(plot)
  
}

#--------------------------------------------------------------------------------------
#finished per week resolution

finishedIssuesByResolution <- aggregate(x=finishedIssues$Quantity, by=list(finishedIssues$StartWeek, finishedIssues$status), sum)
#View(finishedIssuesByResolution)

#finishedIssuesByResolution <- finishedIssuesByResolution[order(finishedIssuesByResolution$week,]

colnames(finishedIssuesByResolution)[1] <- "week"
colnames(finishedIssuesByResolution)[2] <- "resolution"
colnames(finishedIssuesByResolution)[3] <- "resolved"
# 
# #View(defectsReportedWeeklyByCause)
# # #stop()
# 
# 
ggplot(data = finishedIssuesByResolution, aes(x = finishedIssuesByResolution$week, y = finishedIssuesByResolution$resolved, 
                                              fill = finishedIssuesByResolution$resolution)) + 
  geom_bar(stat="identity") + 
  ggtitle("Issues resolved by resolution weekly") + scale_fill_manual( values = c25 )+
  ylab("Number of bugs") +
  xlab("week")

#--------------------------------------------------------------------------------------------

#uncomment for pdf  
#dev.off()