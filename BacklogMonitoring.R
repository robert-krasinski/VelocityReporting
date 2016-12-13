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

getLatestFile <- function(filePattern)
{
  files <- list.files(path = "/Users/robertk/Office365/OneDrive - Kainos Software Ltd/Documents/VelocityReporting/data/", pattern = filePattern)
  files <- sort(files, decreasing = TRUE)
  latestFile <- paste("/Users/robertk/Office365/OneDrive - Kainos Software Ltd/Documents/VelocityReporting/data/", files[1], sep = "")
  
  return(latestFile)
}


latestFile <- getLatestFile("VelocityIssues2016.*.csv")
issues <- read.csv(
  file= latestFile,
  head=TRUE,sep=",", dec=".", stringsAsFactors=FALSE)

issues <- subset(issues, select=-c(id, sprint, updated, priority, 
                                     severity, component, created,
                                     reporter, devOwner,
                                     workTimeToComplete, timeToComplete,
                                     movedToComplete,	aggregatetimeoriginalestimate,
                                     timeoriginalestimate,	transitions,	remainingEstimate))

latestFile <- getLatestFile("VelocityVXTAndEpic.*.csv")
vxtEpic <- read.csv(
  file= latestFile,
  head=TRUE,sep=",", dec=".", stringsAsFactors=FALSE)

vxtEpic <- subset(vxtEpic, select=-c(id, sprint, updated, priority, 
                                     severity, component, created,
                                     reporter, devOwner,
                                     workTimeToComplete, timeToComplete,
                                     movedToComplete,	aggregatetimeoriginalestimate,
                                     timeoriginalestimate,	transitions,	remainingEstimate))
vxtEpic$summary <- gsub("/", "", vxtEpic$summary)
vxtEpic <- vxtEpic[!vxtEpic$fixVersion %in% c('0.2 ', 'POC ', 
                                              'Post ITH Go-Live 1.4 ', 'Post ITH Go-Live 1.3 ',
                                              'Post ITH Go-Live 1.1 ', 'Post ITH Go-Live 1.2 ',
                                              'Post ITH Go-Live '),]
#View(vxtEpic)
#stop()

latestFile <- getLatestFile("EpicIssue.*.csv")
epicIssue <- read.csv(
  file= latestFile,
  head=TRUE,sep=",", dec=".", stringsAsFactors=FALSE)
#View(epicIssue)
#stop()

vxtIssues <- vxtEpic[vxtEpic$project == 'VXT',]
vxtIssues <- vxtIssues[!vxtIssues$status %in% c('Done', 'Rejected'),]
vxtIssues <- vxtIssues[order(vxtIssues$fixVersion),]

#View(vxtIssues)
#stop()

epics <- vxtEpic[vxtEpic$project != 'VXT',]

epicsAndIssues <- rbind(epics, issues)
epics <- epics[epics$key == 'VBS-1150',]

#epicsAndIssues <- epicsAndIssues[!epics$status %in% c('Done', 'Rejected'),]
#View(epics)
#stop()




backlog <- merge(vxtIssues, epicIssue, by.x ="key", by.y = "epic", all.x = TRUE)
backlog <- merge(backlog, epicsAndIssues, by.x = 'issue', by.y = 'key', all.x = TRUE, suffixes = c('.vxt', '.child1'))
backlog$key.child1 <- backlog$issue

epicIssueNoVXT <- epicIssue[substr(epicIssue$epic, 1, 3) != 'VXT',]
epicIssueNoVXT <- epicIssueNoVXT[substr(epicIssueNoVXT$issue, 1, 3) != 'VXT',]
#View(epicIssueNoVXT)
#stop()

backlog <- merge(backlog, epicIssueNoVXT, by.x ="key.child1", by.y = "epic", all.x = TRUE, suffixes = c('.child1.1', '.child2'))
#backlog$key.child2 <- backlog$issue.x
View(backlog)
stop()

backlog$pathString <- paste("VXT", 
                            backlog$fixVersion.vxt, 
                            paste(backlog$key, backlog$summary.vxt, backlog$status.vxt, sep = ", "),
                            backlog$minorVersion.child1,
                            paste(backlog$key.child1, backlog$type.child1, backlog$summary.child1, 
                                  backlog$status.child1, sep = ", ") , sep = "/")

#View(backlog)
#stop()


backlogTree <- as.Node(backlog)
print(backlogTree, limit = 200)
#plot(backlogTree)

