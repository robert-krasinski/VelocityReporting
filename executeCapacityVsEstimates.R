#remove all previous data from environment
rm(list=ls(all=TRUE)) 

#load data from excel
#change this for your local path to capacity excel
capacityExcel <- "/Users/robertk/Box\ Sync/Capacity\ Plan/Velocity\ Platform\ Capacity\ Plan\ V0.2.xlsx"
if(!file.exists(capacityExcel))
{
   print(paste("File:", capacityExcel, "not exist. Stopping execution."))
}

source('./CapacityVsEstimates.R')