## To run this from the shell prompt, type:
#  R --vanilla < eveCSAnalysisTrigger.R


### Load necessary libraries and dependent files

# files
source("./eveCSAnalysisTriggerSQLQueries.R")
source("./eveCSAnalysisTriggerProcessCycle.R")

### Code version
trigger_code_version="'CSTrigger v1.1'"

### Logging
logsPath <- "./logs/"
date <- Sys.Date()
formatteddate <- format(date, format="%Y_%m_%d")
logfile <- paste(logsPath, formatteddate, "_eve_cs_analysis_trigger.log", sep="")

### Load libraries for the MySQL database
require(DBI, quietly = TRUE)
library(RMySQL) # MySQL library for queries
drv <- dbDriver("MySQL")
### Open a connection to the MySQL database
# con <- dbConnect(drv, host="cledwall.dcs.aber.ac.uk", dbname ="robot_scientist_v5", user="paramgen", password="paramgenpw")
con <- dbConnect(drv, host="localhost", dbname ="robot_scientist_v5", user="paramgen", password="paramgenpw")

## log for testing
timenow <- format(Sys.time(), "%Y %m %d %X")
sink(logfile, append=T)
print("=============================")
print(paste(timenow, " : Code version = ", trigger_code_version, sep=""))
sink(NULL)

### Select counts of cherry screen studies per cycle with study status PARAMETERS
### and with no still running studies
avail_cherry_scrn_cycles <- select_eve_cs_cycles_rdy_for_analysis()

if(!is.list(avail_cherry_scrn_cycles)) {
  timenow <- format(Sys.time(), "%Y %m %d %X")
  sink(logfile, append=T)
  print(paste(timenow, " : Failed to fetch cherry screen ids available", sep=""))
  sink(NULL)
} else {

num_cycles <- dim(avail_cherry_scrn_cycles)[1]

# check if there are cycles to process
if(!is.null(num_cycles) && num_cycles > 0) {
 ## log for testing
 timenow <- format(Sys.time(), "%Y %m %d %X")
 sink(logfile, append=T)
 print("------------------------------------------------------------------")
 print(paste(timenow, " : Number of cherry screen cycles available for analysis = ", num_cycles, sep=""))
 sink(NULL)

 ## print out to appear in email
 print("AUTOMATED EVE CHERRY SCREEN ANALYSIS REPORT")
 print("============================================")
 print(paste(timenow, " : Trigger code version = ", trigger_code_version, sep=""))
 print("------------------------------------------------------------------")
 print(paste(timenow, " : Number of cherry screen cycles identified for analysis = ", num_cycles, sep=""))
 print("(N.B. one cycle = many assay plates)")
 
 successfullcyclecnt=0
 failedcyclecnt=0

 ## process each cherry screen cycle 
 for (i in 1:num_cycles){
   idcherryscreen <- avail_cherry_scrn_cycles[i,1]

   ## log for testing
   timenow <- format(Sys.time(), "%Y %m %d %X")
   sink(logfile, append=T)
   print("------------------------------------------------------------------")
   print(paste(timenow, " : Processing cherry screen id : ", idcherryscreen, sep=""))
   sink(NULL)

   ## print out to appear in email
   print("------------------------------------------------------------------")
   print(paste(timenow, " : Processing cherry screen id : ", idcherryscreen, sep=""))
   
   # Run the method to process a single cherry screen
   successflag <- generate_csv_file_for_cherry_screen(idcherryscreen)

   # Check flag on return
   if(successflag == T) {
     # increment counter
     successfullcyclecnt <- successfullcyclecnt+1
     
     ## log success
     timenow <- format(Sys.time(), "%Y %m %d %X")
     sink(logfile, append=T)
     print("------------------------------------------------------------------")
     print(paste(timenow, " : Successfully processed cherry screen id : ", idcherryscreen, sep=""))
     sink(NULL)

     ## print out to appear in email
     print("------------------------------------------------------------------")
     print(paste(timenow, " : Cherry screen id ", idcherryscreen, " was successfully processed.", sep=""))
     
   } else {
     # increment counter
     failedcyclecnt <- failedcyclecnt+1
     
     ## log for testing
     timenow <- format(Sys.time(), "%Y %m %d %X")
     sink(logfile, append=T)
     print("------------------------------------------------------------------")
     print(paste(timenow, " : Failed to process cherry screen id : ", idcherryscreen, sep=""))
     sink(NULL)

     ## print out to appear in email
     print("------------------------------------------------------------------")
     print(paste(timenow, " : Cherry screen id ", idcherryscreen, " FAILED processing.", sep=""))
    
   } # end else for check of success flag
 } # end cherry screen cycles loop

 ## log for testing
 timenow <- format(Sys.time(), "%Y %m %d %X")
 sink(logfile, append=T)
 print(paste(timenow, " : Cherry screen processing totals : ", sep=""))
 print(paste(timenow, " : Successfully processed cycles = ", successfullcyclecnt, sep=""))
 print(paste(timenow, " : Failed cycles = ", failedcyclecnt, sep=""))
 sink(NULL)
 
 ## print out for display in email
 print("------------------------------------------------------------------")
 print(paste(timenow, " : Cherry screen processing totals : ", sep=""))
 print(paste(timenow, " : Successfully processed cycles = ", successfullcyclecnt, sep=""))
 print(paste(timenow, " : Failed cycles = ", failedcyclecnt, sep=""))
 if(failedcyclecnt > 0) {
   print(paste(timenow, " : Refer to logs in parameter generation directory on server for more detailed error information.", sep=""))
 } 
 
} else {
 ## no cycles to process
 timenow <- format(Sys.time(), "%Y %m %d %X")
 sink(logfile, append=T)
 print(paste(timenow, " : No cherry screen cycles identified to process.", sep=""))
 sink(NULL)
} # end cycles loop

} # end else select available cycles

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
