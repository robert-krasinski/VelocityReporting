# first try Windows CP1252, although that's almost surely not supported on Mac:
Sys.setlocale("LC_ALL", "UTF-8") # Make sure not to omit the `"LC_ALL",` first argument, it will fail.
#Sys.setlocale("LC_ALL", "UTF-8") # the name might need to be 'CP1252'

# next try IS08859-1(/'latin1'), this works for me:
#Sys.setlocale("LC_ALL", "pt_PT.ISO8859-1")

# Try "pt_PT.UTF-8" too...

# in your program, make sure the Sys.setlocale worked, sprinkle this assertion in your code before attempting to read.csv:
#stopifnot(Sys.getlocale('LC_CTYPE') == "pt_PT.ISO8859-1")
library(xlsx)
library(treemap)
library(data.tree)

rm(list=ls(all=TRUE)) 
projects <- c('VEL', 'VBS', 'VIN')

getLatestFile <- function(filePattern)
{
  files <- list.files(path = "./data/", pattern = filePattern)
  files <- sort(files, decreasing = TRUE)
  latestFile <- paste("./data/", files[1], sep = "")
  
  return(latestFile)
}

latestFile <- getLatestFile("linkedVxt.*.csv")
linkedVXT <- read.csv(
  file= latestFile,
  head=TRUE,sep=",", dec=".", stringsAsFactors=FALSE)

linkedVXT <- linkedVXT[startsWith(linkedVXT$linkedKey, 'VXT'),]

latestFile <- getLatestFile("VelocityIssues.*.csv")
issues <- read.csv(
  file= latestFile,
  head=TRUE,sep=",", dec=".", stringsAsFactors=FALSE)

issues <- subset(issues, select=-c(id, sprint, updated, priority, 
                                     severity, component, created,
                                     reporter, devOwner,
                                     workTimeToComplete, timeToComplete,
                                     movedToComplete,	aggregatetimeoriginalestimate,
                                     timeoriginalestimate,	transitions,	remainingEstimate))

latestFile <- getLatestFile("VXTAndRelated.*.csv")
vxtAndRelated <- read.csv(
  file= latestFile,
  head=TRUE,sep=",", dec=".", stringsAsFactors=FALSE)

vxtAndRelated <- subset(vxtAndRelated, select=-c(id, sprint, updated, priority, 
                                     severity, component, created,
                                     reporter, devOwner,
                                     workTimeToComplete, timeToComplete,
                                     movedToComplete,	aggregatetimeoriginalestimate,
                                     timeoriginalestimate,	transitions,	remainingEstimate,
                                     codeReviewToDev, testsToDev))
vxtAndRelated$summary <- gsub("/", "", vxtAndRelated$summary)

#remove vxts with not applicable versions
vxtAndRelated <- vxtAndRelated[!(vxtAndRelated$fixVersion %in% c('0.2 ','0.3 ', 'POC ', 
                                              'Post ITH Go-Live 1.4 ', 'Post ITH Go-Live 1.3 ',
                                              'Post ITH Go-Live 1.1 ', 'Post ITH Go-Live 1.2 ',
                                              'Post ITH Go-Live ', '2.0 ', '3.0 ', 'Activiti ',
                                              '') & vxtAndRelated$project == 'VXT'),]
#do not display rejected issues
vxtAndRelated <- vxtAndRelated[vxtAndRelated$status != 'Rejected',]
#vxtAndRelated <- vxtAndRelated[vxtAndRelated$key == 'VWF-1699',]

#View(vxtAndRelated)
#stop()

latestFile <- getLatestFile("RelatedIssues.*.csv")
issueLinks <- read.csv(
  file= latestFile,
  head=TRUE,sep=",", dec=".", stringsAsFactors=FALSE)
#View(issueLinks)
#stop()

vxtIssues <- vxtAndRelated[vxtAndRelated$project == 'VXT',]
vxtIssues <- vxtIssues[!vxtIssues$status %in% c('Done', 'Rejected'),]
vxtIssues <- vxtIssues[order(vxtIssues$fixVersion),]

#View(vxtIssues)
#stop()

#epics <- vxtEpic[vxtEpic$project != 'VXT',]

#relatedIssues <- vxtAndRelated
#epics <- epics[epics$key == 'VBS-1545',]

#relatedIssues <- relatedIssues[!epics$status %in% c('Done', 'Rejected'),]
#relatedIssues <- relatedIssues[relatedIssues$key == 'II-778',]
#View(relatedIssues)
#stop()

source('./sprintsLib.R')
sprints <- loadSprints()

sprintIssueFiles <- list.files(path = "./data/", pattern = "SprintIssue.*.csv")
sprintIssueFiles <- sort(sprintIssueFiles, decreasing = TRUE)

latestSprintIssueFile <- paste("./data/", sprintIssueFiles[1], sep = "")
print(latestSprintIssueFile)
sprintIssues <- read.csv(
  file= latestSprintIssueFile,
  head=TRUE,sep=",", dec=".", stringsAsFactors=FALSE)



createBacklogNodes3 <- function()
{
  issueLinksFiltered <- issueLinks[!grepl('VXT',issueLinks$linkedKey),]
  issueLinksFiltered <- issueLinksFiltered[!grepl('VMCM',issueLinksFiltered$key),]
  issueLinksFiltered <- issueLinksFiltered[!grepl('VMCM',issueLinksFiltered$linkedKey),]
  issueLinksFiltered <- issueLinksFiltered[!grepl('ITH',issueLinksFiltered$linkedKey),]
  issueLinksFiltered <- issueLinksFiltered[!grepl('ITH',issueLinksFiltered$key),]
  #View(issueLinksFiltered)
  #stop()
  
  backlog <- merge(vxtIssues, issueLinksFiltered, by.x ="key", by.y = "key", all.x = TRUE, suffixes = c('.vxt', '.child1'))
  
  #View(backlog)
  #stop()
  #vxtAndRelatedTmp <- vxtAndRelated[vxtAndRelated$key == 'VEL-1541',]
  #View(vxtAndRelatedTmp)
  #stop()
  
  backlog <- merge(backlog, vxtAndRelated, by.x = 'linkedKey', by.y = 'key', all.x = TRUE, suffixes = c('.vxt', '.child1'))
  backlog$key.child1 <- backlog$linkedKey
  
  
  backlog <- merge(backlog, issueLinksFiltered, by.x ="key.child1", by.y = "key", all.x = TRUE, suffixes = c('.child1', '.child2'))
  
  #backlogTmp <- backlog[backlog$key == 'VXT-190',]
  #issueLinksFilteredTmp <- issueLinksFiltered[issueLinksFiltered$key == 'VDO-198',] 
  #View(issueLinksFilteredTmp)
  #stop()
  
  backlog <- merge(backlog, vxtAndRelated, by.x = 'linkedKey.child2', by.y = 'key', all.x = TRUE, suffixes = c('.child1', '.child2'))
  
  colnames(backlog)[which(names(backlog) == "minorVersion")] <- "minorVersion.child2"
  colnames(backlog)[which(names(backlog) == "linkedKey.child2")] <- "key.child2"
  colnames(backlog)[which(names(backlog) == "fixVersion")] <- "fixVersion.child2"
  colnames(backlog)[which(names(backlog) == "type")] <- "type.child2"
  colnames(backlog)[which(names(backlog) == "status")] <- "status.child2"
  colnames(backlog)[which(names(backlog) == "project")] <- "project.child2"
  colnames(backlog)[which(names(backlog) == "summary")] <- "summary.child2"
  
  #backlog$minorVersion.child2 <- backlog$minorVersion
  #backlog$key.child2 <- backlog$
  
  #backlog$key.child1
  #backlogTmp <- subset(backlog, select=c(linkedkey.child1))
  #View(backlog)
  #stop()
  
  
  
  
  
  
  #View(backlog)
  #stop()
  
  #according to this so question. apostrophes can break plot
  #http://stackoverflow.com/questions/40401045/large-data-tree-causes-plot-to-error
  backlog$summaryCleaned.vxt <- gsub("['\"/%-,_]", " ", backlog$summary.vxt)
  backlog$summaryCleaned.child1 <- gsub("['\"/%-,_]", " ", backlog$summary.child1)
  #backlog$summaryCleaned.vxt <- gsub("/", " ", backlog$summaryCleaned.vxt)
  #backlog$summaryCleaned.child1 <- gsub("/", " ", backlog$summaryCleaned.child1)
  backlog$summaryCleaned.child2 <- gsub("['\"/%-,_]", " ", backlog$summary.child2)
  #backlog$summaryCleaned.child2 <- gsub("/", " ", backlog$summaryCleaned.child2)
  #backlog$summaryCleaned.child1 <- gsub("'", " ", backlog$summary.child1)
  
  #View(backlog)
  backlog <- backlog[order(backlog$fixVersion.vxt),]
  backlog$pathString <- paste("VXT", 
                              backlog$fixVersion.vxt, 
                              paste(backlog$key, backlog$summaryCleaned.vxt, backlog$status.vxt, sep = ", "),
                              backlog$fixVersion.child1,
                              paste(backlog$key.child1, backlog$type.child1, backlog$summaryCleaned.child1, backlog$status.child1, sep = ", "),
                              backlog$fixVersion.child2,
                              paste(backlog$key.child2, backlog$type.child2, backlog$summaryCleaned.child2, backlog$status.child2, sep = ", "),
                              sep = "/")
  #backlog$pathString <- gsub("['\"/%-,_]", " ", backlog$pathString)
  backlog$pathString <- gsub("NA", "", backlog$pathString)
  
  #backlog <- head(backlog,1)
  write.xlsx(backlog, sheetName = "data", append = FALSE,
             "./data/backlogTree.xlsx") 
  
  #options(error=stop)
  #View(backlog)
  #stop()
  backlog <- backlog[backlog$fixVersion.vxt == '1.1 CURRENT ',]
  
  #backlog <- backlog[backlog$fixVersion.vxt %in% c('1.1a ', '1.1b ', '1.1c ', '1.1d ', '1.1e ', '1.1f '),]
  #View(backlog)
  #stop()
    
  
  #stop()
  
  backlogTree <- as.Node(backlog)
  
}



GetNodeShape <- function(node){
  if(grepl( c("Epic"), node$name)) return("ellipse")
  return("underline")
} 

GetNodeColor <- function(node){
  
  if(grepl( c("Awaiting Prioritisation"), node$name)) return("red")
  if(grepl( c("Idea"), node$name)) return("red")
  if(grepl( c("Backlog"), node$name)) return("red")
  if(grepl( c("Tech. Scoping"), node$name)) return("red")
  if(grepl( c("Refinement"), node$name)) return("red")
  if(grepl( c("Completed"), node$name)) return("green")
  if(grepl( c("Done"), node$name)) return("green")
  if(grepl( c("Awaiting Review"), node$name)) return("green")
  if(grepl( c("In Progress"), node$name)) return("orange")
  if(grepl( c("In Development"), node$name)) return("orange")
  if(grepl( c("In Testing"), node$name)) return("orange")
  if(grepl( c("In Code review"), node$name)) return("orange")
  if(grepl( c("Ready to Test"), node$name)) return("orange")
  
  
  return("black")
  
} 

plotVersionTree <- function(){
  #9
  backlogTree <- createBacklogNodes3()
  
  print(backlogTree, limit = 1000)
  
  SetGraphStyle(backlogTree, rankdir = "LR")
  
  SetNodeStyle(backlogTree, fontname = 'helvetica', shape = GetNodeShape, color = GetNodeColor)
  plot(backlogTree)
}

#plotVersionTree()

#warnings()
#stop()

backlogIssues <- issues[!issues$status %in% c('Completed', 'Awaiting Review', 'Rejected', 'Idea', 'Frozen'),]
backlogIssues <- backlogIssues[backlogIssues$type != 'Sub-task',]

#View(backlogIssues)
#stop()
#backlogIssues <- backlogIssues[backlogIssues$fixVersion == '1.0 - ITH Live',]
backlogIssues <- merge(backlogIssues, linkedVXT, by.x = 'key', by.y = 'key', 
                       all.x = TRUE, suffixes = c('.backlog', '.vxt'))

backlogIssues$islinkedVXT <- ifelse(backlogIssues$type == 'Bug', 'Bug',
                                    ifelse(!is.na(backlogIssues$linkedKey), 'linked VXT', 'not linked VXT'))
backlogIssues$count <- 1

backlogIssuesAggr <- aggregate( x=cbind(backlogIssues$count), 
                                 by=list(backlogIssues$project, 
                                         backlogIssues$islinkedVXT),  
                                 FUN = sum)
colnames(backlogIssuesAggr )[1] <- "project"
colnames(backlogIssuesAggr )[2] <- "isLinked"
#colnames(backlogIssuesAggr )[3] <- "type"
colnames(backlogIssuesAggr )[3] <- "count"

backlogIssuesAggr$isLinked <- paste(backlogIssuesAggr$isLinked, ":" ,
                                    backlogIssuesAggr$count)

#View(backlogIssuesAggr)
#stop()

for (currentProject in projects) {
  backlogIssuesAggrPerProject <- backlogIssuesAggr[backlogIssuesAggr$project == currentProject,]
  plot <- pie(backlogIssuesAggrPerProject$count, 
              labels = backlogIssuesAggrPerProject$isLinked, 
              col = c("red", "green", "orange"),
              main=paste("Issues in backlog for project:", currentProject, "in all versions."))
  print(plot)
}
#stop()
# colnames(backlogIssuesAggr )[1] <- "project"
# colnames(backlogIssuesAggr )[2] <- "linkedVXT"
# colnames(backlogIssuesAggr )[3] <- "issuesCount"
# backlogIssuesAggr$notLinked <- backlogIssuesAggr$issuesCount - backlogIssuesAggr$linkedVXT
# 
# 
#stop()


backlogIssues$backlogStatus <- ifelse(backlogIssues$status == 'Idea', "Idea", 
                                      ifelse(backlogIssues$status %in% c('Refinement', 'Tech Refinement', 'Estimation'), 'Refinement', 
                                             ifelse(backlogIssues$status == 'Backlog', 'Ready', 'In Progress')))
  
#View(backlogIssues)
#stop()

backlogIssuesAggrStatus <- aggregate( x=cbind(backlogIssues$count), 
                                by=list(backlogIssues$project, 
                                        backlogIssues$backlogStatus),  
                                FUN = sum)
colnames(backlogIssuesAggrStatus )[1] <- "project"
colnames(backlogIssuesAggrStatus )[2] <- "backlogStatus"
#colnames(backlogIssuesAggr )[3] <- "type"
colnames(backlogIssuesAggrStatus )[3] <- "count"

backlogIssuesAggrStatus$backlogStatusLabel <- paste(backlogIssuesAggrStatus$backlogStatus, ":" ,
                                                    backlogIssuesAggrStatus$count)

#View(backlogIssuesAggrStatus)
#stop()

for (currentProject in projects) {
  backlogIssuesAggrStatusPerProject <- backlogIssuesAggrStatus[backlogIssuesAggrStatus$project == currentProject,]
  plot <- pie(backlogIssuesAggrStatusPerProject$count, 
              labels = backlogIssuesAggrStatusPerProject$backlogStatusLabel, 
              #col = c("orange", "blue",),
              main=paste("Issues in backlog for project:", currentProject, "in all versions"))
  print(plot)
}



#----------------------------------------------------------------------------------------------------------
#visualise future sprints completeness
sprintsWithIssues <- merge(sprintIssues, backlogIssues, by="key", all.x = TRUE)
sprintsWithIssues <- sprintsWithIssues[sprintsWithIssues$status != 'Rejected',]
#temp <- sprintsWithIssues[sprintsWithIssues$sprintId == '330',]
#View(temp)
#stop()
sprintsWithIssues <- merge(sprintsWithIssues, sprints, by.x ="sprintId", by.y = "id", all.x = TRUE, all.y = TRUE)
futureSprintsIssues <- sprintsWithIssues[sprintsWithIssues$state == 'future',]

futureSprintsIssues <- futureSprintsIssues[!is.na(futureSprintsIssues$key),]
#View(futureSprintsIssues)
#stop()

 backlogIssuesAgrPerSprint <- aggregate( x=cbind(futureSprintsIssues$count), 
                                         by=list(futureSprintsIssues$project, futureSprintsIssues$name,
                                                 futureSprintsIssues$backlogStatus),  
                                         FUN = sum)
 
 colnames(backlogIssuesAgrPerSprint )[1] <- "project"
 colnames(backlogIssuesAgrPerSprint )[2] <- "sprintName"
 #colnames(backlogIssuesAggr )[3] <- "type"
 colnames(backlogIssuesAgrPerSprint )[3] <- "status"
 colnames(backlogIssuesAgrPerSprint )[4] <- "count"
 
 
 for(currentProject in projects)
 {
   backlogIssuesAgrPerSprintCurrProject <- backlogIssuesAgrPerSprint[backlogIssuesAgrPerSprint$project == currentProject,]
   
   plot <- ggplot(data = backlogIssuesAgrPerSprintCurrProject, 
                  aes(x = backlogIssuesAgrPerSprintCurrProject$sprintName,
                      y = backlogIssuesAgrPerSprintCurrProject$count,
                      fill = backlogIssuesAgrPerSprintCurrProject$status,
                      label = backlogIssuesAgrPerSprintCurrProject$count)) +
     geom_bar(stat="identity") +
     ggtitle(paste("Future sprints status for project:", currentProject)) + 
     #scale_fill_manual( values = c("yellow", "orange") ) +
     ylab("Number of issues") +
     xlab("Sprint name") +
     labs(fill  = "Status") +
     geom_text(size = 3, position = position_stack(vjust = 0.5)) +
     theme(axis.text.x = element_text(angle = 90, hjust = 1))
   
   print(plot)
 }
 
 #View(backlogIssuesAgrPerSprint)
 #stop() 
 
 #---------------------------------------------------------------------------------------------------------- 
 futureSprintsAggr <- aggregate( x=cbind(futureSprintsIssues$count), 
                                         by=list(futureSprintsIssues$project, futureSprintsIssues$name,
                                                 futureSprintsIssues$islinkedVXT),  
                                         FUN = sum)
 
 colnames(futureSprintsAggr )[1] <- "project"
 colnames(futureSprintsAggr )[2] <- "sprintName"
 #colnames(backlogIssuesAggr )[3] <- "type"
 colnames(futureSprintsAggr )[3] <- "linkStatus"
 colnames(futureSprintsAggr )[4] <- "count"
 
 for(currentProject in projects)
 {
   futureSprintsAggrCurrProject <- futureSprintsAggr[futureSprintsAggr$project == currentProject,]
   #View(futureSprintsAggrCurrProject)
   plot <- ggplot(data = futureSprintsAggrCurrProject, 
                  aes(x = futureSprintsAggrCurrProject$sprintName,
                      y = futureSprintsAggrCurrProject$count,
                      fill = futureSprintsAggrCurrProject$linkStatus,
                      label = futureSprintsAggrCurrProject$count)) +
     geom_bar(stat="identity") +
     ggtitle(paste("Future sprints status for project:", currentProject)) + 
     #scale_fill_manual( values = c("yellow", "orange") ) +
     ylab("Number of issues") +
     xlab("Sprint name") +
     labs(fill  = "Status") +
     geom_text(size = 3, position = position_stack(vjust = 0.5)) +
     theme(axis.text.x = element_text(angle = 90, hjust = 1))
   
   print(plot)
 }
 
 #View(futureSprintsAggr)
#----------------------------------------------------------------------------------------------------------
write.xlsx(backlogIssues, sheetName = "issues", append = FALSE,
           "./data/backlog.xlsx") 
