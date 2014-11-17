## To run this from the shell prompt, type:
#  R --vanilla < eveAutoParamGen.R


### Load necessary libraries and dependent files
#library(time)

# JJRs files
source("./ProcessEveGrowthCurve.R")

# NDSs files
source("./eveAutoParamGenMySQLQueries.R")
source("./eveAutoParamGenProcessStudy.R")

### Code versions
wrapper_code_version="'AutoPG.R v1.1'"

#PEGC.R v1.3 = SVN rev 1656
#PEGC.R v1.4 = SVN rev 1691
params_code_version="'PEGC.R v1.4'"

### Logging
logsPath <- "./logs/"
date <- Sys.Date()
formatteddate <- format(date, format="%Y_%m_%d")
logfile <- paste(logsPath, formatteddate, "_eve_paramgen_log", sep="")

### Open a connection to the MySQL database
require(DBI, quietly = TRUE)
library(RMySQL) # MySQL library for queries
drv <- dbDriver("MySQL")
### add trycatch here?
#con <- dbConnect(drv, group="autoparamgen", default.file="./mysql.config")
con <- dbConnect(drv, host="192.168.1.20", dbname ="robot_scientist_v5", user="paramgen", password="paramgenpw")

## log for testing
timenow <- format(Sys.time(), "%Y %m %d %X")
sink(logfile, append=T)
print("=============================")
print(paste(timenow, " : Code version = ", wrapper_code_version, sep=""))
sink(NULL)

### Select study IDs available for processing with status 'RUN COMPLETED'
avail_studies <- select_eve_studies_rdy_for_paramgen()

if(!is.list(avail_studies)) {
  timenow <- format(Sys.time(), "%Y %m %d %X")
  sink(logfile, append=T)
  print(paste(timenow, " : Failed to fetch studies available", sep=""))
  sink(NULL)
} else {

num_studies <- dim(avail_studies)[1]

# check there are studies to process
if(!is.null(num_studies) && num_studies > 0) {
 ## log for testing
 timenow <- format(Sys.time(), "%Y %m %d %X")
 sink(logfile, append=T)
 print(paste(timenow, " : Number of studies available for paramgen = ", num_studies, sep=""))
 sink(NULL)

 ## print out to appear in email
 print("AUTOMATED EVE PARAMETER GENERATION REPORT")
 print("=========================================")
 print(paste(timenow, " : Wrapper code version = ", wrapper_code_version, sep=""))
 print(paste(timenow, " : Parameter generation code version = ", params_code_version, sep=""))
 print("------------------------------------------------------------------")
 print(paste(timenow, " : Number of studies identified for parameter generation = ", num_studies, sep=""))
 print("(N.B. one study = one assay plate)")
 
 successfullstudycnt=0
 failedstudycnt=0

 ## process each study 
 for (i in 1:num_studies){
   studyid <- avail_studies[i,1]

   ## log for testing
   timenow <- format(Sys.time(), "%Y %m %d %X")
   sink(logfile, append=T)
   print(paste(timenow, " : Processing study id : ", studyid, sep=""))
   sink(NULL)

   ## print out to appear in email
   print("------------------------------------------------------------------")
   print(paste(timenow, " : Processing study id : ", studyid, sep=""))
   
   # Run the method to process a single study
   successflag <- generate_parameters_for_study(studyid)

   # Check flag on return
   if(successflag == T) {
     # increment counter
     successfullstudycnt <- successfullstudycnt+1
     
     ## log success
     timenow <- format(Sys.time(), "%Y %m %d %X")
     sink(logfile, append=T)
     print(paste(timenow, " : Successfully processed study id : ", studyid, sep=""))
     sink(NULL)

     ## print out to appear in email
     print(paste(timenow, " : Study id ", studyid, " was successfully processed.", sep=""))
     
   } else {
     # increment counter
     failedstudycnt <- failedstudycnt+1
     
     ## log for testing
     timenow <- format(Sys.time(), "%Y %m %d %X")
     sink(logfile, append=T)
     print(paste(timenow, " : Failed to process study id : ", studyid, sep=""))
     sink(NULL)

     ## print out to appear in email
     print(paste(timenow, " : Study id ", studyid, " FAILED processing.", sep=""))
    
   } # end else for check of success flag
 } # end studies loop

 ## log for testing
 timenow <- format(Sys.time(), "%Y %m %d %X")
 sink(logfile, append=T)
 print(paste(timenow, " : Study processing totals : ", sep=""))
 print(paste(timenow, " : Successfully processed studies = ", successfullstudycnt, sep=""))
 print(paste(timenow, " : Failed studies = ", failedstudycnt, sep=""))
 sink(NULL)
 
 ## print out for display in email
 print("------------------------------------------------------------------")
 print(paste(timenow, " : Study processing totals : ", sep=""))
 print(paste(timenow, " : Successfully processed studies = ", successfullstudycnt, sep=""))
 print(paste(timenow, " : Failed studies = ", failedstudycnt, sep=""))
 if(failedstudycnt > 0) {
   print(paste(timenow, " : Refer to logs in parameter generation directory on server for more detailed error information.", sep=""))
 } 
 
} else {
 ## no studies to process
 timenow <- format(Sys.time(), "%Y %m %d %X")
 sink(logfile, append=T)
 print(paste(timenow, " : No studies to process.", sep=""))
 sink(NULL)
} # end studies loop

} # end else select available studies

### disconnect from the database
if(dbDisconnect(con) == FALSE){
  ## failed to close database
  timenow <- format(Sys.time(), "%Y %m %d %X")
  sink(logfile, append=T)
  print(paste(timenow, " : Error: Database failed to disconnect.", sep=""))
  sink(NULL)
}
# end process, display warnings if any
warntext <- warnings()

if(!is.null(warntext) && (length(warntext) > 10)){
  ## log
  sink(logfile, append=T)
  print("------------------------------------------------------------------")
  print("WARNINGS issued from R code : ")
  print(warntext)
  sink(NULL)
  
  ## print for email
  print("------------------------------------------------------------------")
  print("WARNINGS issued from R code : ")
  print(warntext)
}
