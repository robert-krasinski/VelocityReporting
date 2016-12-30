# VelocityReporting
Gathering data from Jira and visualising it

##Prerequisites
Tested with: 
* R version 3.3.0 (2016-05-03)
* Tested with Python 2.7.1
* RStudio Version 0.99.902


##Usage
* Execute VelocityReportsLoadData.py
  * Parameters:
    * --user - Jira username
    * --password - Jira password
    * --jira - path to Jira instance
* Execute (in RStudio) executeCapacityVsEstimates.R
  * You may need to edit executeCapacityVsEstimates.R to correctly point capacity Excel file.
