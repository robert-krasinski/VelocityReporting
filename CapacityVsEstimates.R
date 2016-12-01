library(xlsx)
require(plyr)
require(lubridate)
require(ggplot2)
require("jsonlite")
library(jsonlite)
library(dplyr)
library(data.table)
library(zoo)
#stop()
#require("XLConnect")

rm(list=ls(all=TRUE)) 

#uncomment for pdf
#pdf(file='/Users/robertk/Office365/OneDrive - Kainos Software Ltd/Documents/VelocityReporting/data/CapacityVsEstimates.pdf'
#      , width=15, height=7)

#-----------------------------------------------------------
#load data from excel
capacityExcel <- "/Users/robertk/Box\ Sync/Capacity\ Plan/Velocity\ Platform\ Capacity\ Plan\ V0.2.xlsx"
capacityDate <- read.xlsx(
  capacityExcel, 
  sheetName = "ExportSummary", header = FALSE, rowIndex = 1,
  )
capacityDate <- capacityDate[-1]

capacityDate <- t(capacityDate)
colnames(capacityDate )[1] <- "date"

#View(capacityDate)
#stop()

capacityValues <- read.xlsx(
  capacityExcel, 
  sheetName = "ExportSummary", header = FALSE, rowIndex = c(2,3,4),
)
capacityValues <- capacityValues[-1]
capacityValues <- t(capacityValues)
capacityValues <- as.data.frame(capacityValues)
colnames(capacityValues )[1] <- "VEL"
colnames(capacityValues )[2] <- "VBS"
colnames(capacityValues )[3] <- "VIN"

#View(capacityValues)
#stop()

teamsCapacity <- capacityValues
teamsCapacity$date <- capacityDate
teamsCapacity$date <- as.POSIXct(teamsCapacity$date)

teamsCapacity <- reshape(teamsCapacity, 
             varying = c("VEL", "VBS", "VIN"), 
             v.names = "capacity",
             timevar = "project", 
             times = c("VEL", "VBS", "VIN"), 
             new.row.names = 1:1000,
             direction = "long")

teamsCapacity <- subset(teamsCapacity, select=-c(id)) 


#teamsCapacity <- teamsCapacity[-1]
#teamsCapacity <- t(teamsCapacity)

#teamsCapacity <- as.data.frame(teamsCapacity, stringsAsFactors = FALSE)
#colnames(teamsCapacity )[1] <- "date"
#colnames(teamsCapacity )[2] <- "VEL"
#colnames(teamsCapacity )[3] <- "VBS"
#colnames(teamsCapacity )[4] <- "VIN"
#teamsCapacity$date <- as.POSIXct(as.numeric(teamsCapacity$date), origin="1970-01-01")
#teamsCapacity$V5 <- as.numeric(teamsCapacity$V2)
#head(teamsCapacity)  
#View(teamsCapacity)
#stop()
#-----------------------------------------------------------
#load data from Jira

boards <- c('12', '71', '91')
#boards <- c('12', '71', '91')
sprints <- NULL

for (board in boards) {
  
  sprintJsonFilePattern <- paste("sprints", board, "_.*.json", sep = "")
  sprintFiles <- list.files(path = "/Users/robertk/Office365/OneDrive - Kainos Software Ltd/Documents/VelocityReporting/data/", 
                            pattern = sprintJsonFilePattern)
  
  sprintFiles <- sort(sprintFiles, decreasing = TRUE)
  latestSprintFile <- paste(
    "/Users/robertk/Office365/OneDrive - Kainos Software Ltd/Documents/VelocityReporting/data/", sprintFiles[1], 
    sep = "")
  
  json_data <- fromJSON(txt = latestSprintFile)
  #View(json_data)
  #print(latestSprintFile)
  #stop()
  #boardSprints <- rbind.fill(lapply(json_data$values,function(y){as.data.frame(t(y),stringsAsFactors=FALSE)}))
  #boardSprints <- json_data$values
  boardSprints <- json_data
  
  
  if(is.null(sprints)){
    sprints <- boardSprints
  }else
  {
    #sprints <- mapply(c, sprints, boardSprints)
    sprints <- rbind( sprints, boardSprints)
  }
  
  
}
#stop()

sprints$sprintStartDate <- substr(sprints$startDate, 0, 22)
sprints$sprintStartDate <- as.POSIXct(sprints$sprintStartDate, format="%Y-%m-%dT%H:%M:%S")
sprints$sprintEndDate <- substr(sprints$endDate, 0, 22)
sprints$sprintEndDate <- as.POSIXct(sprints$sprintEndDate, format="%Y-%m-%dT%H:%M:%S")
sprints <- subset(sprints, select=-c(self)) 

#--------------------------------------------------------------------------------------------------------------
#adding future sprint dates 
#VEL
#sprints[sprints$id == 296 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2016-09-15 10:00')
#sprints[sprints$id == 296 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2016-09-28 10:00')
#sprints[sprints$id == 282 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2016-09-29 10:00')
#sprints[sprints$id == 282 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2016-10-12 10:00')
#sprints[sprints$id == 283 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2016-10-13 10:00')
#sprints[sprints$id == 283 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2016-10-26 10:00')
#sprints[sprints$id == 308 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2016-10-27 10:00')
#sprints[sprints$id == 308 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2016-11-9 10:00')
#sprints[sprints$id == 321 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2016-11-10 10:00')
#sprints[sprints$id == 321 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2016-11-23 10:00')
#sprints[sprints$id == 323 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2016-11-24 10:00')
#sprints[sprints$id == 323 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2016-12-7 10:00')
sprints[sprints$id == 373 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2016-12-8 10:00')
sprints[sprints$id == 373 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2016-12-21 10:00')
sprints[sprints$id == 324 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2016-12-22 10:00')
sprints[sprints$id == 324 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2017-1-4 10:00')
sprints[sprints$id == 325 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2017-1-5 10:00')
sprints[sprints$id == 325 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2017-1-18 10:00')
sprints[sprints$id == 355 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2017-1-19 10:00')
sprints[sprints$id == 355 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2017-2-1 10:00')
#View(sprints)
#stop()
#vbs
#sprints[sprints$id == 269 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2016-09-13 10:00')
#sprints[sprints$id == 269 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2016-09-26 10:00')
#sprints[sprints$id == 277 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2016-09-27 10:00')
#sprints[sprints$id == 277 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2016-10-10 10:00')
#sprints[sprints$id == 278 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2016-10-11 10:00')
#sprints[sprints$id == 278 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2016-10-24 10:00')
#sprints[sprints$id == 279 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2016-10-25 10:00')
#sprints[sprints$id == 279 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2016-11-07 10:00')
#sprints[sprints$id == 280 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2016-11-08 10:00')
#sprints[sprints$id == 280 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2016-11-21 10:00')
#sprints[sprints$id == 328 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2016-11-22 10:00')
#sprints[sprints$id == 328 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2016-12-5 10:00')
sprints[sprints$id == 329 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2016-12-6 10:00')
sprints[sprints$id == 329 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2016-12-19 10:00')
sprints[sprints$id == 330 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2016-12-20 10:00')
sprints[sprints$id == 330 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2017-1-2 10:00')
sprints[sprints$id == 332 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2017-1-3 10:00')
sprints[sprints$id == 332 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2017-1-16 10:00')
sprints[sprints$id == 344 & is.na(sprints$sprintEndDate),]$sprintStartDate <- as.POSIXct('2017-1-17 10:00')
sprints[sprints$id == 344 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2017-1-30 10:00')


#VIN
#sprints[sprints$id == 275 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2016-09-08 10:00')
#sprints[sprints$id == 275 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2016-09-21 10:00')
#sprints[sprints$id == 299 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2016-09-22 10:00')
#sprints[sprints$id == 299 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2016-10-05 10:00')
#sprints[sprints$id == 300 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2016-10-06 10:00')
#sprints[sprints$id == 300 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2016-10-19 10:00')
#sprints[sprints$id == 301 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2016-10-20 10:00')
#sprints[sprints$id == 338 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2016-11-3 10:00')
#sprints[sprints$id == 338 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2016-11-16 10:00')
#sprints[sprints$id == 339 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2016-11-17 10:00')
#sprints[sprints$id == 339 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2016-11-30 10:00')
#sprints[sprints$id == 340 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2016-12-1 10:00')
#sprints[sprints$id == 340 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2016-12-14 10:00')
sprints[sprints$id == 350 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2016-12-15 10:00')
sprints[sprints$id == 350 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2016-12-28 10:00')
sprints[sprints$id == 351 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2016-12-29 10:00')
sprints[sprints$id == 351 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2016-1-11 10:00')



#View(sprints)
#stop()
#sprints$project[boardSprints$originBoardId == 12] <- 'VEL'
#sprints$project[boardSprints$originBoardId == 71] <- 'VBS'
#sprints$project[boardSprints$originBoardId == 91] <- 'VIN'
#sprints <- unlist(sprints)
#View(sprints)
#stop()
#sprintJson_file <- "/Users/robertk/Office365/OneDrive - Kainos Software Ltd/Documents/VelocityReporting/data/sprints12_2016-08-30 16.11.44.824241.json"
#sprintUrl <- 'https://kainos-evolve.atlassian.net/rest/agile/1.0/board/12/sprint'
#json_data <- fromJSON(sprintUrl)
#json_data <- fromJSON(file=json_file)


#sprints <- rbind.fill(lapply(json_data$values,function(y){as.data.frame(t(y),stringsAsFactors=FALSE)}))

#sprints <- as.matrix(json_data$values)
#View(sprints)
#stop()

files <- list.files(path = "/Users/robertk/Office365/OneDrive - Kainos Software Ltd/Documents/VelocityReporting/data/", pattern = "VelocityIssues2016.*.csv")
files <- sort(files, decreasing = TRUE)
latestFile <- paste("/Users/robertk/Office365/OneDrive - Kainos Software Ltd/Documents/VelocityReporting/data/", files[1], sep = "")
issues <- read.csv(
  file= latestFile,
  head=TRUE,sep=",", dec=".", stringsAsFactors=FALSE)


sprintIssueFiles <- list.files(path = "/Users/robertk/Office365/OneDrive - Kainos Software Ltd/Documents/VelocityReporting/data/", pattern = "SprintIssue.*.csv")
sprintIssueFiles <- sort(sprintIssueFiles, decreasing = TRUE)

latestSprintIssueFile <- paste("/Users/robertk/Office365/OneDrive - Kainos Software Ltd/Documents/VelocityReporting/data/", sprintIssueFiles[1], sep = "")
print(latestSprintIssueFile)
sprintIssues <- read.csv(
  file= latestSprintIssueFile,
  head=TRUE,sep=",", dec=".", stringsAsFactors=FALSE)

#sprintIssues <- sprintIssues[sprintIssues$sprintId == '324',]
#View(sprintIssues)
#stop()

completedStatuses <- c('Completed', 'Rejected', 'Reviewed', 'Resolved', 'Closed', 'Awaiting Review', 'Frozen')

#minus3M <- as.Date(Sys.Date()) %m+% months(-3)



#convert to datetime
issues$updated <- as.POSIXct(issues$updated, format="%Y-%m-%dT%H:%M:%S")
issues$created <- as.POSIXct(issues$created, format="%Y-%m-%dT%H:%M:%S")
issues$movedToComplete <- as.POSIXct(issues$movedToComplete, format="%Y-%m-%dT%H:%M:%S")
issues$workDayToComplete <- issues$workTimeToComplete / 8


issues[, "StartMonth"] <- as.Date(cut(as.Date(issues$created), breaks = "month"))
issues[, "EndMonth"] <- as.Date(cut(as.Date(issues$movedToComplete), breaks = "month")) 
issues[, "StartWeek"] <- as.Date(cut(as.Date(issues$created), breaks = "week"))
issues[, "EndWeek"] <- as.Date(cut(as.Date(issues$movedToComplete), breaks = "week"))
issues[, "Quantity"] <- 1

issues$originalEstimation <- issues$timeoriginalestimate / 60 / 60 / 8
issues$originalEstimation[is.na(issues$originalEstimation) ] <- 0

#-----------------------------------------------------------
#issues <- issues[issues$key == 'VEL-1397',]
#View(issues)
#stop()


sprintsWithIssues <- merge(sprintIssues, issues, by="key", all.x = TRUE)
sprintsWithIssues <- sprintsWithIssues[sprintsWithIssues$status != 'Rejected',]
#temp <- sprintsWithIssues[sprintsWithIssues$status == 'Rejected',]
#View(temp)
#stop()
sprintsWithIssues <- merge(sprintsWithIssues, sprints, by.x ="sprintId", by.y = "id", all.x = TRUE, all.y = TRUE)
#sprintsWithIssues <- sprintsWithIssues[sprintsWithIssues$sprintId == 300,]
#View(sprintsWithIssues)
#stop()
#temp <- sprintsWithIssues[sprintsWithIssues$sprintId == 324,]
#View(temp)
#stop()


#sprint id when issue were moved to completed status
sprintsWithIssues$completedSprintId <-  ifelse(!is.na(sprintsWithIssues$movedToComplete) & 
                                           sprintsWithIssues$movedToComplete >= sprintsWithIssues$sprintStartDate &
                                              sprintsWithIssues$movedToComplete <= sprintsWithIssues$sprintEndDate,
                                           sprintsWithIssues$sprintId, NA) 
#add 0 for empty sprints
sprintsWithIssues$originalEstimation <- ifelse(is.na(sprintsWithIssues$originalEstimation), 0, 
                                               sprintsWithIssues$originalEstimation)


#sprintsWithIssues <- sprintsWithIssues[sprintsWithIssues$completedSprintId == '300',]
#sprintsWithIssues <- sprintsWithIssues[!is.na(sprintsWithIssues$completedSprintId),]
#sprintsWithIssues <- sprintsWithIssues[sprintsWithIssues$completedSprintId == '299',]
#sprintsWithIssues <- sprintsWithIssues[sprintsWithIssues$completedSprintId == '277',]
#sprintsWithIssues <- sprintsWithIssues[sprintsWithIssues$sprintId %in% c(10),]
#View(sprintsWithIssues)
#stop()

#completedIssuesPerWeek <- aggregate( x=finishedIssues$Quantity,  by=list(finishedIssues$EndWeek, finishedIssues$project), 
sprintsAggregated <- aggregate( x=sprintsWithIssues$originalEstimation, by=list(sprintsWithIssues$sprintId),  
                                FUN = sum)



colnames(sprintsAggregated )[1] <- "sprintId"
colnames(sprintsAggregated )[2] <- "originalEstimationSum"
#colnames(sprintsAggregated )[2] <- "project"
#sprintsAggregated <- sprintsAggregated[sprintsAggregated$sprintId == 324,]
#View(sprintsAggregated)
#stop()

sprintsDeliveredAggr <- aggregate( x=sprintsWithIssues$originalEstimation, by=list(sprintsWithIssues$completedSprintId),  
                                FUN = sum)

colnames(sprintsDeliveredAggr )[1] <- "sprintId"
colnames(sprintsDeliveredAggr )[2] <- "deliveredEstimationSum"

#View(sprintsDeliveredAggr)
#stop()


sprintsAggregated <- merge(sprintsAggregated, sprintsDeliveredAggr, by ="sprintId", all.x = TRUE, all.y = FALSE)
sprintsAggregated$deliveredEstimationSum <- ifelse(is.na(sprintsAggregated$deliveredEstimationSum), 0, sprintsAggregated$deliveredEstimationSum)
#View(sprintsAggregated)
#stop()


sprintsAggregated <- merge(sprintsAggregated, sprints, by.x ="sprintId", by.y = "id", all.x = TRUE, all.y = FALSE)



sprintsAggregated <- unique(sprintsAggregated)
sprintsAggregated$project <- NA
sprintsAggregated$project <- ifelse(sprintsAggregated$originBoardId %in% c('12', '1'), 'VEL', sprintsAggregated$project)
sprintsAggregated$project <- ifelse(sprintsAggregated$originBoardId %in% c('71', '11'), 'VBS', sprintsAggregated$project)
sprintsAggregated$project <- ifelse(sprintsAggregated$originBoardId == '91', 'VIN', sprintsAggregated$project)
#
#View(sprintsAggregated)
#stop()


#sprintsAggregated$project[sprintsAggregated$originBoardId == 12] <- 'VEL'
#sprintsAggregated$project[sprintsAggregated$originBoardId == 71] <- 'VBS'
#sprintsAggregated$project[sprintsAggregated$originBoardId == 91] <- 'VIN'
#sprintsAggregated <- sprintsAggregated[sprintsAggregated$startDate ]
#sprintsAggregated <- sprintsAggregated[]

sprintsAggregated <- sprintsAggregated[sprintsAggregated$sprintStartDate >= as.POSIXct('2016-06-01'),]
#sprintsAggregated <- sprintsAggregated[sprintsAggregated$state %in% c('active', 'future'),]
#View(sprintsAggregated)
#stop()

sumSprintCapacity <- function(project, sprintStart, sprintEnd)
{
  print(project)
  print(sprintStart)
  print(sprintEnd)
  print(project)
  print(typeof(sprintStart))
  print(typeof(sprintEnd))
  
  capacitySub <- teamsCapacity[teamsCapacity$project == project,]
  
  capacitySub <- capacitySub[capacitySub$date >= sprintStart,]
  capacitySub <- capacitySub[capacitySub$date <= sprintEnd,]
  
                           
  
 
  capacitySum <- sum(capacitySub$capacity)
  print(capacitySum)
  return(capacitySum)
}



#capacity <- sumSprintCapacity(sprintsAggregated[8,]$project, sprintsAggregated[8,]$sprintStartDate, sprintsAggregated[8,]$sprintEndDate)
#print(capacity)
#stop()


sprintsAggregated$sprintCapacity <- apply(sprintsAggregated, 1, function(x) 
  { capacitySum <- sumSprintCapacity(x['project'], 
                      sprintStart = x['sprintStartDate'],
                      sprintEnd = x['sprintEndDate'])
   return(capacitySum)
  
  } )
#View(sprintsAggregated)
#stop()
#sprintsAggregated <- sprintsAggregated[complete.cases(sprintsAggregated),]  
sprintsAggregated <- sprintsAggregated[order(sprintsAggregated$project, sprintsAggregated$sprintStartDate), ]
sprintsAggregated <- sprintsAggregated[sprintsAggregated$sprintCapacity > 0,]
sprintsAggregated$CapacityToEstimationRatio <- ifelse(sprintsAggregated$state == 'closed',
                                                      sprintsAggregated$sprintCapacity / sprintsAggregated$deliveredEstimationSum,
                                                      NA)
#use last value for active and future sprints
#sprintsAggregated$CapacityToEstimationRatio <- na.locf(sprintsAggregated$CapacityToEstimationRatio)
#View(sprintsAggregated)
#stop()

#sprintsAggregated$CapacityToEstimationRatioMeanLast3 <- rollmean(sprintsAggregated$CapacityToEstimationRatio, k = 3, fill = NA, align = "right")
#sprintsAggregated$CapacityToEstimationRatioMaxLast3 <- rollmax(sprintsAggregated$CapacityToEstimationRatio, k = 3, fill = NA, align = "right")
#sprintsAggregated$CapacityToEstimationRatioMinLast3 <- roll(sprintsAggregated$CapacityToEstimationRatio, k = 3, fill = NA, align = "right")






sprintsAggregated$CapacityToEstimationRatioMaxLast3 <- NA
sprintsAggregated$CapacityToEstimationRatioMeanLast3 <- NA
sprintsAggregated$predictedCapacityToEstimationRatio <- NA
#View(sprintsAggregated)
#stop()


projects <- c('VEL', 'VBS', 'VIN')
#projects <- c('VEL')

tempAggr <- NULL
for (currentProject in projects) {
 
  
  sprintsPerProject <- sprintsAggregated[sprintsAggregated$project == currentProject,]
  #sprintsPerProject <- sprintsPerProject[complete.cases(sprintsPerProject),]
  #View(sprintsPerProject)
  #warnistop()
  
  sprintsPerProject$CapacityToEstimationRatioMeanLast3 <- ifelse(sprintsPerProject$state == 'closed',
                                                                 rollmean(sprintsPerProject$CapacityToEstimationRatio, 
                                                                          k = 3, fill = NA, align = "right"),
                                                                 NA)
  
  sprintsPerProject$CapacityToEstimationRatioMaxLast3 <- ifelse(sprintsPerProject$state == 'closed',
                                                                rollmax(sprintsPerProject$CapacityToEstimationRatio, 
                                                                        k = 3, fill = NA, align = "right"),
                                                                NA)
  lastMeanRatio <- tail(na.omit(sprintsPerProject$CapacityToEstimationRatioMeanLast3), 1)
  
  sprintsPerProject$predictedCapacityToEstimationRatio <- ifelse(sprintsPerProject$state %in% c('active',
                                                                                                'future'), 
                                                                 lastMeanRatio, 
                                                                 sprintsPerProject$CapacityToEstimationRatioMaxLast3)
  
  #View(sprintsPerProject)
  #stop()
  if(is.null(tempAggr)) tempAggr <- sprintsPerProject
  else tempAggr <- rbind(tempAggr, sprintsPerProject)
  
}
sprintsAggregated <- tempAggr
#View(tempAggr)
#stop()

sprintsAggregated$deliveredEstimationSum <- ifelse(sprintsAggregated$state %in% c('future'), NA, 
                                                   sprintsAggregated$deliveredEstimationSum)
sprintsAggregated$predictedEstimationSum <- NA

#sprintsAggregated$predictedEstimationSum <- ifelse(sprintsAggregated$state == 'active', sprintsAggregated$originalEstimationSum, 
#                                                   sprintsAggregated$predictedEstimationSum)

sprintsAggregated$predictedEstimationSum <- ifelse(sprintsAggregated$state %in% c('active', 'future', 'closed'), 
                                                   sprintsAggregated$sprintCapacity / sprintsAggregated$predictedCapacityToEstimationRatio,
                                                   sprintsAggregated$predictedEstimationSum)

sprintsAggregated$estimationSum <- ifelse(sprintsAggregated$state %in% c('active', 'future'), 
                                                   sprintsAggregated$originalEstimationSum,
                                                   NA)
#View(sprintsAggregated)
#stop()

sprintsAggregated$estimatedAvailCapacity <- ifelse(sprintsAggregated$state %in% c('active', 'future'), 
                                                   sprintsAggregated$predictedEstimationSum - sprintsAggregated$estimationSum,
                                                   NA)

for (currentProject in projects) {
  sprintsPerProject <- sprintsAggregated[sprintsAggregated$project == currentProject,]
  
  #sprintsPerProject <- sprintsPerProject[complete.cases(sprintsPerProject),]
  
  
  #View(sprintsPerProject)
  #stop()
  
  plot <- ggplot(sprintsPerProject, aes(sprintsPerProject$sprintEndDate)) + 
    #geom_point(aes(y=sprintsPerProject$originalEstimationSum)) +
    #geom_line(aes(y=sprintsPerProject$originalEstimationSum, colour ="original estimates")) +  # first layer
    geom_point(aes(y=sprintsPerProject$deliveredEstimationSum)) +
    geom_line(aes(y=sprintsPerProject$deliveredEstimationSum, colour ="delivered issues (original estimates)")) +  # first layer
    geom_point(aes(y=sprintsPerProject$sprintCapacity)) +
    geom_line(aes(y=sprintsPerProject$sprintCapacity, colour ="team capacity")) + 
    geom_point(aes(y=sprintsPerProject$predictedEstimationSum)) +
    geom_line(aes(y=sprintsPerProject$predictedEstimationSum, colour ="estimated delivery\n(capacity / ratio)")) + 
    geom_point(aes(y=sprintsPerProject$estimationSum)) +
    geom_line(aes(y=sprintsPerProject$estimationSum, colour ="planned estimations (Jira)")) + 
    xlab("sprint end date")+
    ylab("sum of original estimations [d]") +
    ggtitle(paste("Stories delivered during sprint in project", currentProject, sep = " ")) +
    
    labs(fill = "")
  
  print(plot)
  
  plot <- ggplot(sprintsPerProject, aes(sprintsPerProject$sprintEndDate)) + 
    #geom_point(aes(y=sprintsPerProject$originalEstimationSum)) +
    #geom_line(aes(y=sprintsPerProject$originalEstimationSum, colour ="original estimates")) +  # first layer
    geom_point(aes(y=sprintsPerProject$CapacityToEstimationRatio)) +
    geom_line(aes(y=sprintsPerProject$CapacityToEstimationRatio, colour ="Capacity / Delivered Estimations")) +  # first layer
    geom_point(aes(y=sprintsPerProject$CapacityToEstimationRatioMeanLast3)) +
    geom_line(aes(y=sprintsPerProject$CapacityToEstimationRatioMeanLast3, colour ="Capacity / Delivered Estimations\nmean last 3")) +  # first layer
    geom_point(aes(y=sprintsPerProject$CapacityToEstimationRatioMaxLast3)) +
    geom_line(aes(y=sprintsPerProject$CapacityToEstimationRatioMaxLast3, colour ="Capacity / Delivered Estimations\nmax last 3 (pesimistic)")) +  # first layer
    geom_point(aes(y=sprintsPerProject$predictedCapacityToEstimationRatio)) +
    geom_line(aes(y=sprintsPerProject$predictedCapacityToEstimationRatio, colour ="Predicted ratio (mean)")) +  
    
    #geom_point(aes(y=sprintsPerProject$CapacityToEstimationRatioMinLast3)) +
    #geom_line(aes(y=sprintsPerProject$CapacityToEstimationRatioMinLast3, colour ="Capacity / Delivered Estimations\nmin last 3 (optimistic)")) +  # first layer
    xlab("sprint end date")+
    ylab("Ratio") +
    ggtitle(paste("Team capacity / Delivered estimations", currentProject, sep = " ")) +
    
    labs(fill = "")
  
  #print(plot)
}

availCapacityAggr <- sprintsAggregated[sprintsAggregated$state %in% c('active', 'future'),]
availCapacityAggr <- aggregate( x=availCapacityAggr$estimatedAvailCapacity, 
                                by=list(availCapacityAggr$project),  
                                FUN = sum)

colnames(availCapacityAggr )[1] <- "project"
colnames(availCapacityAggr )[2] <- "estimatedAvailableCapacity" 

#View(availCapacityAggr)


#View(sprintsAggregated)



#---------------------------------------------------------------------------------------------
#sum original estimates for all issues in backlog (+dev states) for each team for today
backlog <- issues[issues$status %in% c('Backlog', 'In Development', 'In Code review', 'Ready to Test',
                                       'In Testing'),]
backlog <- backlog[backlog$fixVersion == '1.0 - ITH Live',]

backlogAggr <- aggregate( x=backlog$originalEstimation, by=list(backlog$project),  
                                FUN = sum)
colnames(backlogAggr )[1] <- "project"
colnames(backlogAggr )[2] <- "originalEstimationSum"

#View(backlogAggr)

day <- seq(Sys.Date(), length.out = 365, by = "day")
day <- as.data.frame(day)

burndown <- NULL

for (currentProject in projects) 
{
  tmpDays <- day
  tmpDays$project = currentProject
  #View(tmpDays)
  #stop()
  
  sprintsPerProject <- sprintsAggregated[sprintsAggregated$project == currentProject,]
  lastMeanRatio <- tail(na.omit(sprintsPerProject$CapacityToEstimationRatioMeanLast3), 1)
  tmpDays$CapacityToEstimationRationMeanLast3 <- lastMeanRatio
  
  if(is.null(burndown)) burndown <- tmpDays
  else burndown <- rbind(burndown, tmpDays)
  
  
  
  
  #View(lastMeanRatio)
  #stop()
  
}

burndown <- merge(burndown, backlogAggr, by="project")

burndown$capacity <- apply(burndown, 1, function(x) 
{ capacity <- sumSprintCapacity(x['project'], 
                                   sprintStart = x['day'],
                                   sprintEnd = x['day'])
return(capacity)

} )

burndown$predictedCapacity <- burndown$capacity / burndown$CapacityToEstimationRationMeanLast3

burndownTmp <- NULL

for (currentProject in projects) 
{
  currentBurndown <- burndown[burndown$project == currentProject,]
  currentBurndown$predictedCapacityCumSum <- cumsum(currentBurndown$predictedCapacity)
  #View(currentBurndown)
  #stop()
  
  if(is.null(burndown)) burndownTmp <- currentBurndown
  else burndownTmp <- rbind(burndownTmp, currentBurndown)
}

burndown <- burndownTmp
burndown$backlogLeft <- burndown$originalEstimationSum - burndown$predictedCapacityCumSum
burndown <- burndown[burndown$backlogLeft >= -1,]
#View(burndown)

#calculate sprint dates for future sprints
#calculate sprint capacity for sprint dates
#calculate how sprints will reduce backlog - new backlog size after sprint
#continue until backlog is 0

plannedFinishDate <- as.POSIXct.Date(as.Date('2017-02-21'))
plannedFinishDateLabel <- paste("Planned finish date", 
                                as.Date(plannedFinishDate), sep = " ")


for (currentProject in projects) {
  burndownPerProject <- burndown[burndown$project == currentProject,]
  finishDate <- max(burndownPerProject$day)
  finishDate <- as.POSIXct.Date(finishDate)
  
  capacityToEstimationRatioMeanLast3 <- tail(burndownPerProject$CapacityToEstimationRationMeanLast3, n = 1)
  contingency <- sumSprintCapacity(currentProject, finishDate, plannedFinishDate )
  contingency <- floor(contingency / capacityToEstimationRatioMeanLast3)
  
  #stop()
  contingencyLabel <- paste("Contingency [ideal days]: ", contingency, sep = " ")
  #stop()
  
  #sprintsPerProject <- sprintsPerProject[complete.cases(sprintsPerProject),]
  
  
  #View(sprintsPerProject)
  #stop()
  
  plot <- ggplot(burndownPerProject, aes(burndownPerProject$day)) + 
    #geom_point(aes(y=sprintsPerProject$originalEstimationSum)) +
    #geom_line(aes(y=sprintsPerProject$originalEstimationSum, colour ="original estimates")) +  # first layer
    #geom_point(aes(y=burndownPerProject$backlogLeft)) +
    geom_line(aes(y=burndownPerProject$backlogLeft)) +  # first layer
    #geom_point(aes(y=sprintsPerProject$sprintCapacity)) +
    #geom_line(aes(y=sprintsPerProject$sprintCapacity, colour ="team capacity")) + 
    #geom_point(aes(y=sprintsPerProject$predictedEstimationSum)) +
    #geom_line(aes(y=sprintsPerProject$predictedEstimationSum, colour ="estimated delivery\n(capacity / ratio)")) + 
    #geom_point(aes(y=sprintsPerProject$estimationSum)) +
    #geom_line(aes(y=sprintsPerProject$estimationSum, colour ="planned estimations (Jira)")) + 
    xlab("Day")+
    ylab("Original estimation sum (ideal days)") +
    ggtitle(paste("Burndown chart in project", currentProject, "for", Sys.Date(), sep = " ")) +
    labs(fill = "") +
    geom_vline(xintercept=as.numeric(as.Date(finishDate)), linetype="dotted") + 
    geom_text(mapping=aes(x=as.Date(finishDate), y=5, label=as.Date(finishDate)), size=4, angle=90, vjust=-0.4, hjust=0) + 
    geom_vline(xintercept=as.numeric(as.Date(plannedFinishDate)), linetype="dotted") +
    geom_text(mapping=aes(x=as.Date(plannedFinishDate), y=5, label=plannedFinishDateLabel), size=4, angle=90, vjust=1.2, hjust=0) +
    geom_text(mapping=aes(x=Sys.Date(), y=contingency, label=contingencyLabel), vjust=1.2, hjust=0, family = "Helvetica") +
    geom_hline(yintercept=contingency, linetype="dotted") 
    
    
  print(plot)
}


write.xlsx(sprintsAggregated, sheetName = "data", append = FALSE,
           "/Users/robertk/Office365/OneDrive - Kainos Software Ltd/Documents/VelocityReporting/data/CapacityVsEstimates.xlsx") 
write.xlsx(availCapacityAggr, sheetName = "availableCapacity", append = TRUE,
           "/Users/robertk/Office365/OneDrive - Kainos Software Ltd/Documents/VelocityReporting/data/CapacityVsEstimates.xlsx") 

write.xlsx(burndown, sheetName = "burndown", append = TRUE,
           "/Users/robertk/Office365/OneDrive - Kainos Software Ltd/Documents/VelocityReporting/data/CapacityVsEstimates.xlsx") 



warnings()
#uncomment for pdf  
#dev.off()


