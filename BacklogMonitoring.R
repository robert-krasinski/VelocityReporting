# first try Windows CP1252, although that's almost surely not supported on Mac:
Sys.setlocale("LC_ALL", "UTF-8") # Make sure not to omit the `"LC_ALL",` first argument, it will fail.
#Sys.setlocale("LC_ALL", "UTF-8") # the name might need to be 'CP1252'

# next try IS08859-1(/'latin1'), this works for me:
#Sys.setlocale("LC_ALL", "pt_PT.ISO8859-1")

# Try "pt_PT.UTF-8" too...

# in your program, make sure the Sys.setlocale worked, sprinkle this assertion in your code before attempting to read.csv:
#stopifnot(Sys.getlocale('LC_CTYPE') == "pt_PT.ISO8859-1")

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
  backlog$summaryCleaned.vxt <- gsub("['\"/]", " ", backlog$summary.vxt)
  backlog$summaryCleaned.child1 <- gsub("['\"/]", " ", backlog$summary.child1)
  #backlog$summaryCleaned.vxt <- gsub("/", " ", backlog$summaryCleaned.vxt)
  #backlog$summaryCleaned.child1 <- gsub("/", " ", backlog$summaryCleaned.child1)
  backlog$summaryCleaned.child2 <- gsub("['\"]", " ", backlog$summary.child2)
  #backlog$summaryCleaned.child2 <- gsub("/", " ", backlog$summaryCleaned.child2)
  #backlog$summaryCleaned.child1 <- gsub("'", " ", backlog$summary.child1)
  
  #View(backlog)
  backlog <- backlog[order(backlog$fixVersion.vxt),]
  backlog$pathString <- paste("VXT", 
                              backlog$fixVersion.vxt, 
                              paste(backlog$key, backlog$summaryCleaned.vxt, backlog$status.vxt, sep = ", "),
                              backlog$minorVersion.child1,
                              paste(backlog$key.child1, backlog$type.child1, backlog$summaryCleaned.child1, backlog$status.child1, sep = ", "),
                              backlog$minorVersion.child2,
                              paste(backlog$key.child2, backlog$type.child2, backlog$summaryCleaned.child2, backlog$status.child2, sep = ", "),
                              sep = "/")
  backlog$pathString <- gsub("NA", "", backlog$pathString)
  
  
  backlog <- backlog[backlog$fixVersion.vxt == '1.0 - ITH Live 0.21 ',]
  #View(backlog)
  #stop()
  
  #stop()
  backlogTree <- as.Node(backlog)
}

backlogTree <- createBacklogNodes3()
print(backlogTree, limit = 200)


SetGraphStyle(backlogTree, rankdir = "LR")

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

SetNodeStyle(backlogTree, fontname = 'helvetica', shape = GetNodeShape, color = GetNodeColor)
plot(backlogTree)


#warnings()
#stop()

backlogIssues <- issues[!issues$status %in% c('Completed', 'Awaiting Review', 'Rejected'),]
backlogIssues <- backlogIssues[backlogIssues$type != 'Sub-task',]
backlogIssues <- backlogIssues[backlogIssues$fixVersion == '1.0 - ITH Live',]
backlogIssues <- merge(backlogIssues, linkedVXT, by.x = 'key', by.y = 'key', 
                       all.x = TRUE, suffixes = c('.backlog', '.vxt'))

backlogIssues$islinkedVXT <- ifelse(!is.na(backlogIssues$linkedKey), 'linked VXT', 'not linked VXT')
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
              col = c("green", "red"),
              main=paste("Issues in backlog for project:", currentProject, "in version 1.0"))
  print(plot)
}

# colnames(backlogIssuesAggr )[1] <- "project"
# colnames(backlogIssuesAggr )[2] <- "linkedVXT"
# colnames(backlogIssuesAggr )[3] <- "issuesCount"
# backlogIssuesAggr$notLinked <- backlogIssuesAggr$issuesCount - backlogIssuesAggr$linkedVXT
# 
# 
#stop()

write.xlsx(backlogIssues, sheetName = "issues", append = FALSE,
           "./data/backlog.xlsx") 
