if(!exists("capacityExcel")) print ("capacityExcel variable doesn't exist. Please execute executeCapacityVsEstimates.R.")

library(xlsx)
require(plyr)
require(lubridate)
require(ggplot2)
require("jsonlite")
library(jsonlite)
library(dplyr)
library(data.table)
library(zoo)
library(scales)

#stop()
#require("XLConnect")

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


#uncomment for pdf
#pdf(file='/Users/robertk/Office365/OneDrive - Kainos Software Ltd/Documents/VelocityReporting/data/CapacityVsEstimates.pdf'
#      , width=15, height=7)

#-----------------------------------------------------------

source('./sprintsLib.R')

sprints <- loadSprints()
#View(sprints)
#stop()

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
#View(teamsCapacity)
#stop()
teamsCapacity <- reshape(teamsCapacity, 
             varying = c("VEL", "VBS", "VIN"), 
             v.names = "capacity",
             timevar = "project", 
             times = c("VEL", "VBS", "VIN"), 
             new.row.names = 1:2000,
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

files <- list.files(path = "./data/", pattern = "VelocityIssues.*.csv")
files <- sort(files, decreasing = TRUE)
latestFile <- paste("./data/", files[1], sep = "")
issues <- read.csv(
  file= latestFile,
  head=TRUE,sep=",", dec=".", stringsAsFactors=FALSE)


sprintIssueFiles <- list.files(path = "./data/", pattern = "SprintIssue.*.csv")
sprintIssueFiles <- sort(sprintIssueFiles, decreasing = TRUE)

latestSprintIssueFile <- paste("./data/", sprintIssueFiles[1], sep = "")
print(latestSprintIssueFile)
sprintIssues <- read.csv(
  file= latestSprintIssueFile,
  head=TRUE,sep=",", dec=".", stringsAsFactors=FALSE)

#sprintIssues <- sprintIssues[sprintIssues$sprintId == '330',]
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

issues$remainingEstimate <- issues$remainingEstimate / 60 / 60 / 8
issues$remainingEstimate[is.na(issues$remainingEstimate) ] <- 0

#-----------------------------------------------------------
#issues <- issues[issues$key == 'VEL-1397',]
#View(issues)
#stop()


sprintsWithIssues <- merge(sprintIssues, issues, by="key", all.x = TRUE)
sprintsWithIssues <- sprintsWithIssues[sprintsWithIssues$status != 'Rejected',]
#temp <- sprintsWithIssues[sprintsWithIssues$sprintId == '330',]
#View(temp)
#stop()
sprintsWithIssues <- merge(sprintsWithIssues, sprints, by.x ="sprintId", by.y = "id", all.x = TRUE, all.y = TRUE)
#sprintsWithIssues <- sprintsWithIssues[sprintsWithIssues$sprintId == 330,]
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
sprintsAggregated <- aggregate( x=cbind(sprintsWithIssues$originalEstimation, 
                                        sprintsWithIssues$remainingEstimate), by=list(sprintsWithIssues$sprintId),  
                                FUN = sum)



colnames(sprintsAggregated )[1] <- "sprintId"
colnames(sprintsAggregated )[2] <- "originalEstimationSum"
colnames(sprintsAggregated )[3] <- "remainingEstimationSum"
#colnames(sprintsAggregated )[2] <- "project"
#sprintsAggregated <- sprintsAggregated[sprintsAggregated$sprintId == 330,]
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



projects <- c('VEL', 'VBS', 'VIN')
#projects <- c('VEL')

tempAggr <- NULL
for (currentProject in projects) {
 
  
  sprintsPerProject <- sprintsAggregated[sprintsAggregated$project == currentProject,]
  #sprintsPerProject <- sprintsPerProject[complete.cases(sprintsPerProject),]
  
  #warnistop()
  sprintsPerProject$CapacityToEstimationRatioMeanLast3 <- rollapplyr(
    sprintsPerProject$CapacityToEstimationRatio,list(-(3:1)),mean,fill=NA)
  sprintsPerProject$CapacityToEstimationRatioMeanLast3 <- ifelse(sprintsPerProject$state == 'closed',
                                                                 sprintsPerProject$CapacityToEstimationRatioMeanLast3,
                                                                 NA)
  
  sprintsPerProject$CapacityToEstimationRatioSDLast3 <- rollapplyr(
    sprintsPerProject$CapacityToEstimationRatio,list(-(3:1)),sd,fill=NA)
  sprintsPerProject$CapacityToEstimationRatioSDLast3 <- ifelse(sprintsPerProject$state == 'closed',
                                                                 sprintsPerProject$CapacityToEstimationRatioSDLast3,
                                                                 NA)
   
  sprintsPerProject$CapacityToEstimationRatioMaxLast3 <- rollapplyr(
    sprintsPerProject$CapacityToEstimationRatio,list(-(3:1)),max,fill=NA)
  
  sprintsPerProject$CapacityToEstimationRatioMedianLast3 <- rollapplyr(
    sprintsPerProject$CapacityToEstimationRatio,list(-(3:1)),median,fill=NA)
  
  sprintsPerProject$CapacityToEstimationRatioMaxLast3 <- ifelse(sprintsPerProject$state == 'closed',
                                                                sprintsPerProject$CapacityToEstimationRatioMaxLast3,
                                                                NA)
  #View(sprintsPerProject)
  #stop()
  lastMeanRatio <- tail(na.omit(sprintsPerProject$CapacityToEstimationRatioMeanLast3), 1)
  lastMeanRatioSD <-  tail(na.omit(sprintsPerProject$CapacityToEstimationRatioSDLast3), 1)
  #lastMeanRatioPlusSd <- lastMeanRatio + lastMeanRatioSD
  #lastMeanRatioSubSd <- lastMeanRatio - lastMeanRatioSD
  lastMaxRatio <- tail(na.omit(sprintsPerProject$CapacityToEstimationRatioMaxLast3), 1)
  
  sprintsPerProject$predictedCapacityToEstimationRatio <- ifelse(sprintsPerProject$state %in% c('active',
                                                                                                'future'), 
                                                                 lastMeanRatio, 
                                                                 sprintsPerProject$CapacityToEstimationRatioMeanLast3)
  
  sprintsPerProject$CapacityToEstimationRatioMeanLast3 <- ifelse(sprintsPerProject$state %in% c('active',
                                                                                                'future'), 
                                                                 lastMeanRatio, 
                                                                 sprintsPerProject$CapacityToEstimationRatioMeanLast3)

    sprintsPerProject$CapacityToEstimationRatioSDLast3 <- ifelse(sprintsPerProject$state %in% c('active',
                                                                                              'future'), 
                                                               lastMeanRatioSD, 
                                                               sprintsPerProject$CapacityToEstimationRatioSDLast3)
  
  
#  View(sprintsPerProject)
#  stop()
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
sprintsAggregated$estimationSum <- ifelse(sprintsAggregated$state %in% c('active', 'future'), 
                                          sprintsAggregated$originalEstimationSum,
                                          NA)

sprintsAggregated$predictedEstimationSum <- ifelse(sprintsAggregated$state %in% c('active', 'future', 'closed'), 
                                                   sprintsAggregated$sprintCapacity / sprintsAggregated$predictedCapacityToEstimationRatio,
                                                   sprintsAggregated$predictedEstimationSum)

sprintsAggregated$predictedSumMaxModel <- ifelse(sprintsAggregated$state %in% c('active', 'future', 'closed'), 
                                                   sprintsAggregated$sprintCapacity / sprintsAggregated$CapacityToEstimationRatioMaxLast3,
                                                   sprintsAggregated$predictedEstimationSum)


sprintsAggregated$predictedSumMeanModel <- ifelse(sprintsAggregated$state %in% c('active', 'future', 'closed'), 
                                                   sprintsAggregated$sprintCapacity / sprintsAggregated$CapacityToEstimationRatioMeanLast3,
                                                   sprintsAggregated$predictedEstimationSum)

sprintsAggregated$predictedSumMeanModelAddSd <-  sprintsAggregated$sprintCapacity / 
                                                    (sprintsAggregated$CapacityToEstimationRatioMeanLast3 + sprintsAggregated$CapacityToEstimationRatioSDLast3)
sprintsAggregated$predictedSumMeanModelSubSd <-  sprintsAggregated$sprintCapacity / 
  (sprintsAggregated$CapacityToEstimationRatioMeanLast3 - sprintsAggregated$CapacityToEstimationRatioSDLast3)

sprintsAggregated$predictedSumMedianModel <- ifelse(sprintsAggregated$state %in% c('active', 'future', 'closed'), 
                                                  sprintsAggregated$sprintCapacity / sprintsAggregated$CapacityToEstimationRatioMedianLast3,
                                                  sprintsAggregated$predictedEstimationSum)


sprintsAggregated$remainingEstimationSum <- ifelse(sprintsAggregated$state %in% c('active', 'future'), 
                                          sprintsAggregated$remainingEstimation,
                                          NA)


sprintsAggregated$estimatedAvailCapacity <- ifelse(sprintsAggregated$state %in% c('active', 'future'), 
                                                   sprintsAggregated$predictedEstimationSum - sprintsAggregated$remainingEstimationSum,
                                                   NA)

#View(sprintsAggregated)
#stop()
#calculate estimation model error
#maxModel - ratio = max from last 3
#meanModel - ratio = mean from last 3

sprintsAggregated$maxModelError <- round(sprintsAggregated$deliveredEstimationSum - 
                                                sprintsAggregated$predictedSumMaxModel, digits = 2)

sprintsAggregated$meanModelError <- round(sprintsAggregated$deliveredEstimationSum - 
                                           sprintsAggregated$predictedSumMeanModel, digits = 2)

sprintsAggregated$medianModelError <- round(sprintsAggregated$deliveredEstimationSum - 
                                            sprintsAggregated$predictedSumMedianModel, digits = 2)


sprintsAggregatedClosedSprints <- sprintsAggregated[!is.na(sprintsAggregated$maxModelError), ]
estimationModelError <- aggregate( x=cbind(sprintsAggregatedClosedSprints$maxModelError,
                                           sprintsAggregatedClosedSprints$meanModelError,
                                           sprintsAggregatedClosedSprints$medianModelError), by=list(sprintsAggregatedClosedSprints$project),
                               FUN = sum)
colnames(estimationModelError )[1] <- "project"
colnames(estimationModelError )[2] <- "cumMaxModelError" 
colnames(estimationModelError )[3] <- "cumMeanModelError"
colnames(estimationModelError )[4] <- "cumMedianModelError" 

estimationModelErrorSD <- aggregate( x=cbind(sprintsAggregatedClosedSprints$maxModelError,
                                           sprintsAggregatedClosedSprints$meanModelError,
                                           sprintsAggregatedClosedSprints$medianModelError), by=list(sprintsAggregatedClosedSprints$project),
                                   FUN = sd)
colnames(estimationModelErrorSD )[1] <- "project"
colnames(estimationModelErrorSD )[2] <- "cumMaxModelSD" 
colnames(estimationModelErrorSD )[3] <- "cumMeanModelSD"
colnames(estimationModelErrorSD )[4] <- "cumMedianModelSD" 

#meanErrors <- sprintsAggregated[sprintsAggregated$project == 'VBS',]$meanModelError
#summary(meanErrors)

#estimationModelError 
estimationModelError <- merge(estimationModelError, estimationModelErrorSD, by="project")
#View(estimationModelError)
#stop()


for (currentProject in projects) {
  sprintsPerProject <- sprintsAggregated[sprintsAggregated$project == currentProject,]

  
  #sprintsPerProject <- sprintsPerProject[complete.cases(sprintsPerProject),]
  
  
  
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
    #geom_line(aes(y=sprintsPerProject$predictedSumMedianModel, colour ="median model")) + 
    geom_line(aes(y=sprintsPerProject$predictedSumMeanModelSubSd, colour ="mean - sd")) + 
    geom_line(aes(y=sprintsPerProject$predictedSumMeanModelAddSd, colour ="mean + sd")) + 
    xlab("sprint end date")+
    ylab("sum of original estimations [d]") +
    ggtitle(paste("Stories delivered during sprint in project", currentProject, sep = " ")) +
    scale_fill_manual( values = c25 ) +    
    labs(fill = "") +
    scale_x_datetime(date_breaks = "1 month", date_labels = "%b") +
    ylim(c(0,100))
  
  print(plot)
  
  #View(sprintsPerProject)
  #stop()
  # if(currentProject == 'VBS')
  # {
  #   View(sprintsPerProject)
  #   stop()
  # }
  
  # plot <- ggplot(sprintsPerProject, aes(sprintsPerProject$sprintEndDate)) + 
  #   #geom_point(aes(y=sprintsPerProject$originalEstimationSum)) +
  #   #geom_line(aes(y=sprintsPerProject$originalEstimationSum, colour ="original estimates")) +  # first layer
  #   geom_point(aes(y=sprintsPerProject$CapacityToEstimationRatio)) +
  #   geom_line(aes(y=sprintsPerProject$CapacityToEstimationRatio, colour ="Capacity / Delivered Estimations")) +  # first layer
  #   geom_point(aes(y=sprintsPerProject$CapacityToEstimationRatioMeanLast3)) +
  #   geom_line(aes(y=sprintsPerProject$CapacityToEstimationRatioMeanLast3, colour ="Capacity / Delivered Estimations\nmean last 3")) +  # first layer
  #   geom_point(aes(y=sprintsPerProject$CapacityToEstimationRatioMaxLast3)) +
  #   geom_line(aes(y=sprintsPerProject$CapacityToEstimationRatioMaxLast3, colour ="Capacity / Delivered Estimations\nmax last 3 (pesimistic)")) +  # first layer
  #   geom_point(aes(y=sprintsPerProject$predictedCapacityToEstimationRatio)) +
  #   geom_line(aes(y=sprintsPerProject$predictedCapacityToEstimationRatio, colour ="Predicted ratio (mean)")) +  
  #   
  #   #geom_point(aes(y=sprintsPerProject$CapacityToEstimationRatioMinLast3)) +
  #   #geom_line(aes(y=sprintsPerProject$CapacityToEstimationRatioMinLast3, colour ="Capacity / Delivered Estimations\nmin last 3 (optimistic)")) +  # first layer
  #   xlab("sprint end date")+
  #   ylab("Ratio") +
  #   ggtitle(paste("Team capacity / Delivered estimations", currentProject, sep = " ")) +
  #   
  #   labs(fill = "")
  
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
backlog <- issues[issues$status %in% c('QA Refinement', 'Estimation', 'Tech Refinement', 'Backlog', 'In Development', 'In Code review', 'Ready to Test',
                                       'In Testing'),]
#backlog <- issues[issues$key == 'VIN-356',]
#backlog <- issues[issues$minorVersion %in% c('UK', 'Gloucester'),]

backlog <- backlog[backlog$fixVersion %in% c('1.1'),]
#backlog <- backlog[backlog$project == 'VEL',]
#View(backlog)
#stop()



backlogAggr <- aggregate( x=cbind(backlog$originalEstimation, backlog$remainingEstimate), by=list(backlog$project),  
                                FUN = sum)

colnames(backlogAggr )[1] <- "project"
colnames(backlogAggr )[2] <- "originalEstimationSum"
colnames(backlogAggr )[3] <- "remainingEstimateSum"

#View(backlogAggr)
#stop()

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
  hist(sprintsPerProject$meanModelError, main = paste("Histogram of mean model error for project" , currentProject),
       xlab = "Delivered - Estimated [d]")
  lastMeanRatio <- tail(na.omit(sprintsPerProject$CapacityToEstimationRatioMeanLast3), 1)
  lastMeanRatioSd <- tail(na.omit(sprintsPerProject$CapacityToEstimationRatioSDLast3), 1)
  tmpDays$CapacityToEstimationRationMeanLast3 <- lastMeanRatio
  tmpDays$CapacityToEstimationRatioSdLast3 <- lastMeanRatioSd
  
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
burndown$predictedCapacityAddSd <- burndown$capacity / 
  (burndown$CapacityToEstimationRationMeanLast3 + 2 * burndown$CapacityToEstimationRatioSdLast3)
burndown$predictedCapacitySubSd <- burndown$capacity / 
  (burndown$CapacityToEstimationRationMeanLast3 - 2 * burndown$CapacityToEstimationRatioSdLast3)

#View(burndown)
#stop()

burndownTmp <- NULL

for (currentProject in projects) 
{
  currentBurndown <- burndown[burndown$project == currentProject,]
  currentBurndown$predictedCapacityCumSum <- cumsum(currentBurndown$predictedCapacity)
  currentBurndown$predictedCapacityCumSumAddSd <- cumsum(currentBurndown$predictedCapacityAddSd)
  currentBurndown$predictedCapacityCumSumSubSd <- cumsum(currentBurndown$predictedCapacitySubSd)
  #View(currentBurndown)
  #stop()
  
  if(is.null(burndown)) burndownTmp <- currentBurndown
  else burndownTmp <- rbind(burndownTmp, currentBurndown)
}

burndown <- burndownTmp
burndown$backlogLeft <- burndown$remainingEstimateSum - burndown$predictedCapacityCumSum
burndown$backlogLeftAddSd <- burndown$remainingEstimateSum - burndown$predictedCapacityCumSumAddSd
burndown$backlogLeftSubSd <- burndown$remainingEstimateSum - burndown$predictedCapacityCumSumSubSd

burndown <- burndown[burndown$backlogLeftAddSd >= -2,]
burndown$backlogLeft <- ifelse(burndown$backlogLeft >= 0 
                                  | (shift(burndown$backlogLeft, 1L, type="lag") >= 0 & burndown$backlogLeft <= 0), 
                                  burndown$backlogLeft, NA)
#burndown$backlogLeftTmp <- ifelse(burndown$backlogLeft >= 0, burndown$backlogLeft, NA)
burndown$backlogLeftAddSd <- ifelse(burndown$backlogLeftAddSd >= 0 
                                    | (shift(burndown$backlogLeftAddSd, 1L, type="lag") >= 0 & burndown$backlogLeftAddSd <= 0), 
                                    burndown$backlogLeftAddSd, NA)
burndown$backlogLeftSubSd <- ifelse(burndown$backlogLeftSubSd >= 0 
                                    | (shift(burndown$backlogLeftSubSd, 1L, type="lag") >= 0 & burndown$backlogLeftSubSd <= 0), 
                                    burndown$backlogLeftSubSd, NA)
#View(burndown)
#stop()

#calculate sprint dates for future sprints
#calculate sprint capacity for sprint dates
#calculate how sprints will reduce backlog - new backlog size after sprint
#continue until backlog is 0

plannedFinishDate <- as.POSIXct.Date(as.Date('2017-02-15'))
plannedFinishDateLabel <- paste("Planned 1.0 Go live date", 
                                as.Date(plannedFinishDate), sep = " ")


for (currentProject in c('VBS', 'VEL')) {
  burndownPerProject <- burndown[burndown$project == currentProject,]
  finishDate <- max(burndownPerProject[!is.na(burndownPerProject$backlogLeft),]$day)
  finishDate <- as.POSIXct.Date(finishDate)
  
  capacityToEstimationRatioMeanLast3 <- tail(burndownPerProject$CapacityToEstimationRationMeanLast3, n = 1)
  if(finishDate < plannedFinishDate) difference <- sumSprintCapacity(currentProject, finishDate, plannedFinishDate )
  else difference <- sumSprintCapacity(currentProject, plannedFinishDate, finishDate )
  
  difference <- round(abs(difference / capacityToEstimationRatioMeanLast3), digits = 1)
  
  #stop()
  if(finishDate < plannedFinishDate)
    differenceLabel <- paste("Contingency [ideal days]: ", difference, sep = " ")
  else 
    differenceLabel <- paste("Difference [ideal days]: ", difference, sep = " ")
  
  #stop()
  
  #sprintsPerProject <- sprintsPerProject[complete.cases(sprintsPerProject),]
  
  
  #View(sprintsPerProject)
  #stop()
  
  plot <- ggplot(burndownPerProject, aes(burndownPerProject$day)) + 
    scale_fill_manual( values = c25 ) + 
    #geom_point(aes(y=sprintsPerProject$originalEstimationSum)) +
    #geom_line(aes(y=sprintsPerProject$originalEstimationSum, colour ="original estimates")) +  # first layer
    #geom_point(aes(y=burndownPerProject$backlogLeft)) +
    geom_line(aes(y=burndownPerProject$backlogLeft, colour = "realistic (mean ratio last 3 sprints)")) + 
    geom_line(aes(y=burndownPerProject$backlogLeftSubSd, colour = "optimistic (mean ratio - 2 * sd)")) +  
    geom_line(aes(y=burndownPerProject$backlogLeftAddSd, colour = "pesimistic (mean ratio + 2 * sd)")) +  
    
    #geom_point(aes(y=sprintsPerProject$sprintCapacity)) +
    #geom_line(aes(y=sprintsPerProject$sprintCapacity, colour ="team capacity")) + 
    #geom_point(aes(y=sprintsPerProject$predictedEstimationSum)) +
    #geom_line(aes(y=sprintsPerProject$predictedEstimationSum, colour ="estimated delivery\n(capacity / ratio)")) + 
    #geom_point(aes(y=sprintsPerProject$estimationSum)) +
    #geom_line(aes(y=sprintsPerProject$estimationSum, colour ="planned estimations (Jira)")) + 
    xlab("Day")+
    ylab("Remaining estimation sum (ideal days)") +
    ggtitle(paste("Burndown chart in project", currentProject, "for", Sys.Date(), sep = " ")) +
    labs(fill = "") +
    geom_vline(xintercept=as.numeric(as.Date(finishDate)), linetype="dotted") + 
    geom_text(mapping=aes(colour = "realistic (mean ratio last 3 sprints)", x=as.Date(finishDate), y=5, label=as.Date(finishDate)), size=3, angle=90, vjust=-0.4, hjust=0) 
    #geom_vline(xintercept=as.numeric(as.Date(plannedFinishDate)), linetype="dotted") +
    #geom_text(mapping=aes(x=as.Date(plannedFinishDate), y=5, label=plannedFinishDateLabel), size=3, angle=90, vjust=1.2, hjust=0) +
    # +
    geom_hline(yintercept=difference, linetype="dotted") #+
    #scale_x_datetime(date_breaks = "1 week", date_labels = "%b")
    
    #plot = plot + geom_text(mapping=aes(x=Sys.Date(), y=difference, label=differenceLabel), vjust=1.2, hjust=0, family = "Helvetica")
    
  print(plot)
}




write.xlsx(sprintsAggregated, sheetName = "data", append = FALSE,
           "./data/CapacityVsEstimates.xlsx") 
write.xlsx(availCapacityAggr, sheetName = "availableCapacity", append = TRUE,
           "./data/CapacityVsEstimates.xlsx") 

write.xlsx(burndown, sheetName = "burndown", append = TRUE,
           "./data/CapacityVsEstimates.xlsx") 



warnings()
#uncomment for pdf  
#dev.off()


