loadSprints <- function()
{
  boards <- c('12', '71', '91')
  #boards <- c('12', '71', '91')
  sprints <- NULL
  
  for (board in boards) {
    
    sprintJsonFilePattern <- paste("sprints", board, "_.*.json", sep = "")
    sprintFiles <- list.files(path = "./data/", 
                              pattern = sprintJsonFilePattern)
    
    sprintFiles <- sort(sprintFiles, decreasing = TRUE)
    latestSprintFile <- paste(
      "./data/", sprintFiles[1], 
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
  
  sprints <- subset(sprints, select=-c(self, goal)) 
  #View(sprints)
  #stop()
  #
  
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
  #sprints[sprints$id == 373 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2016-12-8 10:00')
  #sprints[sprints$id == 373 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2016-12-21 10:00')
  #sprints[sprints$id == 324 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2016-12-22 10:00')
  #sprints[sprints$id == 324 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2017-1-4 10:00')
  #sprints[sprints$id == 325 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2017-1-5 10:00')
  #sprints[sprints$id == 325 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2017-1-18 10:00')
  #sprints[sprints$id == 408 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2017-1-18 10:00')
  #sprints[sprints$id == 408 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2017-1-31 10:00')
  #sprints[sprints$id == 325 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2017-1-18 10:00')
  #sprints[sprints$id == 325 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2017-1-31 10:00')
  #sprints[sprints$id == 468 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2017-2-1 10:00')
  #sprints[sprints$id == 468 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2017-2-15 10:00')
  #sprints[sprints$id == 469 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2017-3-1 10:00')
  #sprints[sprints$id == 469 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2017-3-15 10:00')
  sprints <- sprints[sprints$id != 468,]
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
  #sprints[sprints$id == 329 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2016-12-6 10:00')
  #sprints[sprints$id == 329 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2016-12-19 10:00')
  #sprints[sprints$id == 330 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2017-1-3 10:00')
  #sprints[sprints$id == 330 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2017-1-16 10:00')
  #sprints[sprints$id == 332 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2017-1-17 10:00')
  #sprints[sprints$id == 332 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2017-1-30 10:00')
  #sprints[sprints$id == 344 & is.na(sprints$sprintEndDate),]$sprintStartDate <- as.POSIXct('2017-1-31 10:00')
  #sprints[sprints$id == 344 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2017-2-13 10:00')
  #sprints[sprints$id == 467 & is.na(sprints$sprintEndDate),]$sprintStartDate <- as.POSIXct('2017-2-14 10:00')
  #sprints[sprints$id == 467 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2017-2-27 10:00')
  sprints[sprints$id == 488 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2017-3-28 10:00')
  sprints[sprints$id == 488 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2017-4-10 10:00')
  sprints[sprints$id == 506 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2017-4-11 10:00')
  sprints[sprints$id == 506 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2017-4-24 10:00')
  sprints[sprints$id == 524 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2017-4-25 10:00')
  sprints[sprints$id == 524 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2017-5-8 10:00')
  sprints[sprints$id == 525 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2017-5-9 10:00')
  sprints[sprints$id == 525 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2017-5-22 10:00')
  sprints[sprints$id == 526 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2017-5-23 10:00')
  sprints[sprints$id == 526 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2017-6-5 10:00')
  sprints[sprints$id == 527 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2017-6-6 10:00')
  sprints[sprints$id == 527 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2017-6-19 10:00')
  sprints[sprints$id == 528 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2017-6-20 10:00')
  sprints[sprints$id == 528 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2017-7-3 10:00')
  sprints <- sprints[sprints$id != 523,]#UX sprint removed
  #sprint 22 (closed incidentally) removed because it was overwritten by 22(reopened)
  sprints <- sprints[sprints$id != 332,]
  
  
  #View(sprints)
  #stop()
  
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
  #sprints[sprints$id == 350 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2016-12-15 10:00')
  #sprints[sprints$id == 350 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2017-1-4 10:00')
  sprints[sprints$id == 351,]$sprintStartDate <- as.POSIXct('2016-12-15 10:00')
  #sprints[sprints$id == 351 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2017-1-25 10:00')
  #sprints[sprints$id == 356 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2017-1-26 10:00')
  #sprints[sprints$id == 356 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2017-2-8 10:00')
  #sprints[sprints$id == 458 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2017-2-9 10:00')
  #sprints[sprints$id == 458 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2017-2-22 10:00')
  #sprints[sprints$id == 484 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2017-2-23 10:00')
  #sprints[sprints$id == 484 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2017-3-8 10:00')
  sprints[sprints$id == 503 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2017-3-23 10:00')
  sprints[sprints$id == 503 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2017-4-5 10:00')
  sprints[sprints$id == 504 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2017-4-6 10:00')
  sprints[sprints$id == 504 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2017-4-19 10:00')
  sprints[sprints$id == 517 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2017-4-20 10:00')
  sprints[sprints$id == 517 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2017-5-4 10:00')
  sprints[sprints$id == 518 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2017-5-5 10:00')
  sprints[sprints$id == 518 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2017-5-17 10:00')
  sprints[sprints$id == 519 & is.na(sprints$sprintStartDate),]$sprintStartDate <- as.POSIXct('2017-5-18 10:00')
  sprints[sprints$id == 519 & is.na(sprints$sprintEndDate),]$sprintEndDate <- as.POSIXct('2017-5-31 10:00')
  
  
  
  return(sprints)
}