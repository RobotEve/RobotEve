### Eve system parameter generation for a specified study

generate_parameters_for_study <- function(studyid) {

 # minimum number of readings allowed per well growth curve 
 min_num_rdgs_allowed=22

 # check status of this study again, to prevent multiple sets of parameters being
 # generated when multiple cron jobs are running (possible if processing slow enough
 # that another cron job instance runs before first finishes)
 statusok <- select_study_status_ready_to_process(studyid)

 if(statusok == -1){
   timenow <- format(Sys.time(), "%Y %m %d %X")
   sink(logfile, append=T)
   print(paste(timenow, " : Unable to determine if Study in correct status for processing, study ID = ", studyid, sep=""))
   print(paste(timenow, " : Skipping processing this study.",sep=""))
   sink(NULL)
   return(F)
 }
 
 if(statusok == 0){
   timenow <- format(Sys.time(), "%Y %m %d %X")
   sink(logfile, append=T)
   print(paste(timenow, " : WARNING: Study no longer in correct status for processing, study ID = ", studyid, sep=""))
   print(paste(timenow, " : Skipping processing this study.",sep=""))
   sink(NULL)
   return(T)
 }
 
 # if the study status is Ok to proceed, update it to say we're processing it
 newstatusname <- "GEN PARAMETERS"
 ussgp <- update_study_status(studyid, newstatusname)

 # check for success of the update
 if (ussgp == -1) {
   timenow <- format(Sys.time(), "%Y %m %d %X")
   sink(logfile, append=T)
   print(paste(timenow, " : Failed study status update to GEN PARAMETERS for study ID = ", studyid, sep=""))
   print(paste(timenow, " : Cannot continue, failed to update study status in database.",sep=""))
   sink(NULL)
   print("Parameter generation stopped after error updating study status to GEN PARAMETERS. Cannot continue.")
   return(F)
 }
  
 ## log that generating parameters for this study ID
 timenow <- format(Sys.time(), "%Y %m %d %X")
 sink(logfile, append=T)
 string <- paste(timenow, " : Generating parameters for study id : ", studyid, sep="")
 print(string)
 sink(NULL)

 # Eve takes readings using a number of different filters. e.g. Cherry / Sapphire / Venus
 # Generation of parameters for a particular study is done per filter name.
 # First we select plate instance from DB for study ID
 filternames <- character()
 plateInstance = select_eve_plate_instance_for_study_id(studyid)

 if(plateInstance == -1) {
   ## log failure to select plate instance ID for study
   timenow <- format(Sys.time(), "%Y %m %d %X")
   sink(logfile, append=T)
   print(paste(timenow, " : Failed to select plate instance for studyid : ", studyid, sep=""))
   sink(NULL)
   return(F)
 }

 # Fetch investigation name for studyid for display
 investname = select_investname_for_study_id(studyid)
 
 # Fetch studyname for studyid for display
 studyname = select_studyname_for_study_id(studyid)

 # Fetch assay plate barcode for plate instance id for display
 assayplatebc = select_assay_plate_barcode_for_plate_instance(plateInstance)

 ## log details retrieved
 timenow <- format(Sys.time(), "%Y %m %d %X")
 sink(logfile, append=T)
 print(paste(timenow, " : Investigation name = ", investname, sep=""))
 print(paste(timenow, " : Study name = ", studyname, sep=""))
 print(paste(timenow, " : Assay plate instance ID = ", plateInstance, sep=""))
 print(paste(timenow, " : Assay plate barcode = ", assayplatebc, sep=""))
 sink(NULL)
 
 ## printing output for email
 print(paste(timenow, " : Investigation name = ", investname, sep=""))
 print(paste(timenow, " : Study name = ", studyname, sep=""))
 print(paste(timenow, " : Assay plate instance ID = ", plateInstance, sep=""))
 print(paste(timenow, " : Assay plate barcode = ", assayplatebc, sep=""))
 
 # Then we select filternames for the plate instance from the readings table
 # So eventually each well on the plate will have a set of parameters for each reading filter
 filternames <- select_eve_filternames(plateInstance)

 if(!is.list(filternames)) {
   ## log that failed to select filters
   timenow <- format(Sys.time(), "%Y %m %d %X")
   sink(logfile, append=T)
   print(paste(timenow, " : Failed to select filternames for plateInstance : ", plateInstance, sep=""))
   sink(NULL)
   return(F)
 }

 num_filternames=0
 num_filternames <- dim(filternames)[1]

 # counters for current study
 studysuccessfullwellscnt=0
 studyskippedwellscnt=0
 studyfailedwellscnt=0
 
 successcnt=0
 failurecnt=0
 tn_counter=0
 
 # for each filtername
 for (tn_counter in 1:num_filternames) { 
  curFilt <- filternames[tn_counter,1]

  # Select the wells with readings for this plate and filtername
  eve_wells <- select_eve_wells_with_rdgs_for_filter(plateInstance,curFilt)

  # break here?
  
  num_wells <- dim(eve_wells)[1]
  
  ## For each well fetch the data, get params for that well and insert into database. 
  for (i in 1:num_wells) {
   row <- eve_wells[i,1]
   col <- eve_wells[i,2]

   # select well readings for specified well and filter
   well_data <- select_eve_well_readings(plateInstance,curFilt,row,col)
   
   # set up the data for the axes in the growth curve
   curve <- well_data[,2] 
   timesteps <- well_data[,1] 

   # calc number of points in curve, and check if there are enough to proceed
   numpoints <- length(curve)

   #print(paste("Number points in curve for well row = ", row, " col = ", col, " is = ",numpoints, sep=""))

   if (numpoints < min_num_rdgs_allowed) {
     ## log that well is skipped
     timenow <- format(Sys.time(), "%Y %m %d %X")
     sink(logfile, append=T)
     print(paste(timenow, " : There are only ",numpoints," readings for well ", row, col, " on plate ID ", plateInstance," filter:",curFilt, ". Skipping this well.", sep=""))
     sink(NULL)

     ## printing output for email
     timenow <- format(Sys.time(), "%Y %m %d %X")
     print(paste(timenow, " : Skipping well ", row, col, " on plate ID ", plateInstance, " filter:", curFilt, ", only ",numpoints," readings (min is  ", min_num_rdgs_allowed, ")", sep=""))
     

     # increment counter
     studyskippedwellscnt <- studyskippedwellscnt+1
     
     next
   } else {
     # enough readings to continue

     ## log for testing
     ## timenow <- format(Sys.time(), "%Y %m %d %X")
     ## sink(logfile, append=T)
     ## print(paste(timenow, " : There are enough readings (",numpoints,") for plateInstance:",plateInstance," filter:",curFilt," row:",row," col:",col, ". Now attempting to convert timepoints to decimal.", sep=""))
     ## sink(NULL)

     # ensure timestamps are in order
     stimesteps <- sort(as.matrix(timesteps),index.return=T)
     timesteps <- stimesteps$x
     curve <- curve[stimesteps$ix]

     # convert timesteps to timestepsdecimal
     adddiff<- difftime(timesteps[2:numpoints],timesteps[1:numpoints-1],unit="hours")
     adddiff <- c(0,adddiff)
     timestepsdecimal <- matrix(data=0, nrow=numpoints, ncol=1)
     timestepsdecimal[1] <- 0
     for (j in 2:numpoints){
        timestepsdecimal[j] <- timestepsdecimal[j-1] + adddiff[j]
     }
     timestepsdecimal <- as.real(timestepsdecimal)

     curveOD <- curve
     timesteps <- timestepsdecimal
     #print(curveOD)
     #print(timesteps)

     ## log for testing
     ## timenow <- format(Sys.time(), "%Y %m %d %X")
     ## sink(logfile, append=T)
     ## string <- paste(timenow, " : Calculated decimal timesteps data, now calling param generation for plateInstance:",plateInstance," filter:",curFilt," row:",row," col:",col,sep="")
     ## print(string)
     ## sink(NULL)

     # runs the R function for obtaining parameters and creates the 'output' object
     # Log if parameter generation failed
     
     #next code writes data to file before processing
     #string <- paste(investigation,"_",studyid,"_",plateInstance,"_",row,"_",col,sep="")
     #toCalcWellFile <- paste("tobeprocessedwells/", string, ".csv", sep="")
     #write.table(cbind(curveOD,timesteps),file=toCalcWellFile,row.name=F,sep=" ")

     # call parameters function
     toutput <- try(ProcessEveGrowthCurve(curveOD,timesteps,"Eve curve","PLOTSOFF"), silent=T)

     #print("back from param gen, parameters=")
     #print(toutput)

     # check for an error in the parameter generation
     if (inherits(toutput,"try-error")){

       # increment counter
       studyfailedwellscnt <- studyfailedwellscnt+1
       
       ## log error
       timenow <- format(Sys.time(), "%Y %m %d %X")
       sink(logfile, append=T)
       print(paste(timenow, " : Failed parameter generation for plateInstance : ",plateInstance," filter : ",curFilt," row : ",row," col : ",col,sep=""))
       print(paste(timenow, " : Error:",toutput[1],sep=" "))
       print(paste(timenow, " : Now attempting to insert failed row to database.",sep=" "))
       sink(NULL)
     
       # insert a row on the growth parameters table to indicate the parameter generation failed
       ief <- insert_eve_failure(plateInstance,row,col,curFilt)

       if(ief == -1) {
         timenow <- format(Sys.time(), "%Y %m %d %X")
         sink(logfile, append=T)
         print(paste(timenow, " : Failed to insert failed row to database too for plateInstance : ",plateInstance," filter : ",curFilt," row : ",row," col : ",col,sep=""))
         sink(NULL)
       }
       else
       {
        # write the details of the failed well to a file
        failedWell <- paste(studyid,"_",plateInstance,"_",curFilt,"_",row,"_",col,sep="")
        failedWellFile <- paste("failedwells/", failedWell, ".csv", sep="")
        write.table(cbind(curveOD,timesteps),file=failedWellFile,row.name=F,sep=" ")
       }
       next    
      } else {
        # parameter generation appears to have been successful, now insert parameters into database
        output <- toutput
        
        ## log for testing
        ## timenow <- format(Sys.time(), "%Y %m %d %X")
        ## sink(logfile, append=T)
        ## string <- paste(timenow, " : Generated parameters successfully, now attempting database insert for plateInstance:",plateInstance," filter:",curFilt," row:",row," col:",col,sep="")
        ## print(string)
        ## sink(NULL)

        #print(paste("values sending to insert parameters function are PI:",plateInstance," row:",eve_wells[i,1]," col:",eve_wells[i,2]," curFilt:",curFilt,sep=" "))
        #print(output)

        ieq <- insert_eve_query(output, plateInstance, row, col, curFilt)

        # check for success of the insert
        if(ieq == -1) {

          # increment counter
          failurecnt <- failurecnt+1
          
          ## log error
          timenow <- format(Sys.time(), "%Y %m %d %X")
          sink(logfile, append=T)
          print(paste(timenow, " : Failed parameters insert for plateInstance:",plateInstance," filter:",curFilt," row:",row," col:",col,sep=""))
          print(paste(timenow, " : Cannot continue, failed to write parameters to database.",sep=" "))
          sink(NULL)
          # stop("Parameter generation stopped after insert error. Cannot continue.")
          # next
        } else {
          # increment counter
          studysuccessfullwellscnt <- studysuccessfullwellscnt+1
        }
        #dbCommit(con)
        #dbClearResult(iq)
    } # end of else (check for successful param generation)
   } # end of else (case where numpoints was checked)
  } # end for each well
 } # end of for each type of reading (filtername)

 # If successes > 0 update study status to indicate parameters generated
 if(studysuccessfullwellscnt > 0) {
   newstatusname <- "PARAMETERS"
   uss <- update_study_status(studyid, newstatusname)

   # check for success of the update
   if (uss == -1) {
     # print("in section for failed update study status")
   
     timenow <- format(Sys.time(), "%Y %m %d %X")
     sink(logfile, append=T)
     print(paste(timenow, " : Failed study status update to PARAMETERS for study ID = ", studyid))
     print(paste(timenow, " : Cannot continue, failed to update study status in database.",sep=" "))
     sink(NULL)
     stop("Parameter generation stopped after update study status to PARAMETERS error. Cannot continue.")
   }

   ## log counts
   timenow <- format(Sys.time(), "%Y %m %d %X")
   sink(logfile, append=T)
   print(paste(timenow, " : Counts for study id : ", studyid, sep=""))
   print(paste(timenow, " : Curve parameter successes = ", studysuccessfullwellscnt, sep=""))
   print(paste(timenow, " : Curve parameter failures = ", studyfailedwellscnt, sep=""))
   print(paste(timenow, " : Curves skipped for low readings = ", studyskippedwellscnt, sep=""))
   sink(NULL)

   ## print out for email
   print(paste(timenow, " : Curve parameter successes = ", studysuccessfullwellscnt, sep=""))
   print(paste(timenow, " : Curve parameter failures = ", studyfailedwellscnt, sep=""))
   print(paste(timenow, " : Curves skipped for low readings = ", studyskippedwellscnt, sep=""))

 } else {
   # no successfull parameter generation for this study
   # update study status to indicate failure
   newstatusname <- "FAILED"
   ussf <- update_study_status(studyid, newstatusname)

   # check for success of the update
   if (ussf == -1) {
     # print("in section for failed update study status")
   
     timenow <- format(Sys.time(), "%Y %m %d %X")
     sink(logfile, append=T)
     print(paste(timenow, " : Failed study status update to FAILED for study ID = ", studyid))
     print(paste(timenow, " : Cannot continue, failed to update study status in database.",sep=" "))
     sink(NULL)
     stop("Parameter generation stopped after update study status to FAILED error. Cannot continue.")
   }

   ## log counts
   timenow <- format(Sys.time(), "%Y %m %d %X")
   sink(logfile, append=T)
   print(paste(timenow, " : Counts for FAILED study id : ", studyid, sep=""))
   print(paste(timenow, " : Curve parameter successes = ", studysuccessfullwellscnt, sep=""))
   print(paste(timenow, " : Curve parameter failures = ", studyfailedwellscnt, sep=""))
   print(paste(timenow, " : Curves skipped for low readings = ", studyskippedwellscnt, sep=""))
   print("This study has been given the status FAILED in the database as growth parameters could not be calculated. This is a serious error that needs further investigation.")
   sink(NULL)

   ## print out for email
   print(paste(timenow, " : Counts for FAILED study id : ", studyid, sep=""))
   print(paste(timenow, " : Curve parameter successes = ", studysuccessfullwellscnt, sep=""))
   print(paste(timenow, " : Curve parameter failures = ", studyfailedwellscnt, sep=""))
   print(paste(timenow, " : Curves skipped for low readings = ", studyskippedwellscnt, sep=""))
   print("This study has been given the status FAILED in the database as growth parameters could not be calculated. This is a serious error that needs further investigation.")
   
   return(F)
 }

 return(T)
}
