### Eve system parameter generation for a specified cherry screen

generate_csv_file_for_cherry_screen <- function(idcherryscreen) {

 ## log that we are analysing this cherry screen id
 timenow <- format(Sys.time(), "%Y %m %d %X")
 sink(logfile, append=T)
 string <- paste(timenow, " : Triggering analysis of cherry screen id : ", idcherryscreen, sep="")
 print(string)
 sink(NULL)

 avail_studies <- select_studies_for_cherry_screen(idcherryscreen)

 if(!is.list(avail_studies)) {
  timenow <- format(Sys.time(), "%Y %m %d %X")
  sink(logfile, append=T)
  print(paste(timenow, " : Failed to fetch study ids for cherry screen id ",idcherryscreen, sep=""))
  sink(NULL)
  return(F)
  
 } else {
  num_studies <- dim(avail_studies)[1]

  # check if there are studies to process
  if(!is.null(num_studies) && num_studies > 0) {
    ## log for testing
    timenow <- format(Sys.time(), "%Y %m %d %X")
    sink(logfile, append=T)
    print(paste(timenow, " : Number of studies available for analysis = ", num_studies, sep=""))
    sink(NULL)

    # for display in email
    print(paste(timenow, " : Number of studies available for analysis = ", num_studies, sep=""))
    
    # Fetch investigation name for cherry screen id for display
    investname = select_investname_for_cherry_screen_id(idcherryscreen)
    
    # select investigation id for cherry screen id
    idinvestigation <- select_invest_id_for_cherry_screen(idcherryscreen)

    if(idinvestigation == -1) {
      ## log failure to select investigation ID for cherry screen
      timenow <- format(Sys.time(), "%Y %m %d %X")
      sink(logfile, append=T)
      print(paste(timenow, " : Failed to select investigation ID for cherry screen : ", idcherryscreen, sep=""))
      sink(NULL)
      return(F)
    }

    # select cycle number for cherry screen id
    cycle_number<- select_cycle_number_for_cherry_screen(idcherryscreen)

    if(cycle_number == -1) {
      ## log failure to select cycle number for cherry screen
      timenow <- format(Sys.time(), "%Y %m %d %X")
      sink(logfile, append=T)
      print(paste(timenow, " : Failed to select cycle number for cherry screen : ", idcherryscreen, sep=""))
      sink(NULL)
      return(F)
    }
    
    # create csv filename
    timestampnow <- format(Sys.time(), "%Y%m%d%H%M%S")
    csvfilename <- paste("CS_",idinvestigation,"_",cycle_number,"_",idcherryscreen,"_",timestampnow,".csv",sep="")

    ## log details retrieved
    timenow <- format(Sys.time(), "%Y %m %d %X")
    sink(logfile, append=T)
    print(paste(timenow, " : Investigation name = ", investname, sep=""))
    print(paste(timenow, " : ID investigation = ", idinvestigation, sep=""))
    print(paste(timenow, " : Cycle number = ", cycle_number, sep=""))
    print(paste(timenow, " : csv filename = ", csvfilename, sep=""))
    sink(NULL)
    
    # for display in email
    print(paste(timenow, " : Investigation name = ", investname, sep=""))
    print(paste(timenow, " : ID investigation = ", idinvestigation, sep=""))
    print(paste(timenow, " : Cycle number = ", cycle_number, sep=""))
    print(paste(timenow, " : csv filename = ", csvfilename, sep=""))
                
    # create csv file
    csvFilePath <- "./output/"
    csvOutputFile <- paste(csvFilePath,csvfilename, sep="")

    # for each study
    for (i in 1:num_studies){
      idstudy <- avail_studies[i,1]
      
      # Fetch studyname for studyid for display
      studyname = select_studyname_for_study_id(idstudy)

      ## log for testing
      timenow <- format(Sys.time(), "%Y %m %d %X")
      sink(logfile, append=T)
      print("------------------------------------------------------------------")
      print(paste(timenow, " : Processing study id : ", idstudy, sep=""))
      print(paste(timenow, " : Study name : ", studyname, sep=""))
      sink(NULL)

      ## print out to appear in email
      print("------------------------------------------------------------------")
      print(paste(timenow, " : Processing study id : ", idstudy, sep=""))
      print(paste(timenow, " : Study name : ", studyname, sep=""))
      
      # select assay plate wells list for study
      avail_wells <- select_assay_wells_list_for_study(idstudy)

      # counters
      countercompounds=0
      counternegcntrls=0
      counterskipped=0
      
      if(!is.list(avail_wells)) {
        timenow <- format(Sys.time(), "%Y %m %d %X")
        sink(logfile, append=T)
        print(paste(timenow, " : Failed to fetch wells for study id ",idstudy, sep=""))
        sink(NULL)
  
      } else {
        num_wells <- dim(avail_wells)[1]
        
        # check if there are wells to process
        if(!is.null(num_wells) && num_wells > 0) {

          # for each well
          for (i in 1:num_wells) {
            idplateinstance <- avail_wells[i,1]
            wellrow <- avail_wells[i,2]
            wellcol <- avail_wells[i,3]

            # print(paste("Plate id = ",idplateinstance, " Well row = ", wellrow, " Well col = ", wellcol, sep=""))
            # select well content for chemical id != 1313
            compounddata <- select_compound_data_for_assay_well(idplateinstance,wellrow,wellcol)

            if(!is.list(compounddata) || is.null(compounddata[1,2])) {

              # not a compound well, so now see if this is a DMSO negative control well
              negcntrldata <- select_neg_cntrl_data_for_assay_well(idplateinstance,wellrow,wellcol)

              if(is.list(negcntrldata) && !is.null(negcntrldata[1,2])) {

                # well is a negative control and contains only solvent, output neg control row
                sink(csvOutputFile, append=T)
                write.table(negcntrldata, col.names=FALSE, row.names=FALSE, sep = ",")
                sink(NULL)

                # increment counter
                counternegcntrls <- counternegcntrls+1
                
              } else {
                # well contains no content, and is niether a compound nor a negative control well
                # log the details of this well
                timenow <- format(Sys.time(), "%Y %m %d %X")
                sink(logfile, append=T)
                print(paste(timenow," : Plate ", idplateinstance, " Well ", wellrow, wellcol, " has no compound or solvent content.",sep=""))
                sink(NULL)

                # increment counter
                counterskipped <- counterskipped+1
                
              } # end check for row returned from neg control SQL          
              
            } else {

              # well contains a compound, output compound row to csv file
              sink(csvOutputFile, append=T)
              write.table(compounddata, col.names=FALSE, row.names=FALSE, sep = ",")
              sink(NULL)

              # increment counter
              countercompounds <- countercompounds+1
              
            } # end check for row returned from compound SQL
      
          } # end for each well

        } else {
          # no studies to process
          timenow <- format(Sys.time(), "%Y %m %d %X")
          sink(logfile, append=T)
          print(paste(timenow, " : Number of wells zero for cherry screen ID = ", idcherryscreen, " and study ID = ",idstudy, sep=""))
          print(paste(timenow, " : Skipping processing this study ID.",sep=""))
          sink(NULL)
        } # end check num wells

      } # end check avail wells list

      # write out loop counters
      timenow <- format(Sys.time(), "%Y %m %d %X")
      sink(logfile, append=T)
      print("------------------------------------------------------------------")
      print(paste(timenow, " : Processed study id : ", idstudy, sep=""))
      print(paste(timenow, " : Study name : ", studyname, sep=""))
      print(paste(timenow, " : Compound wells found : ",countercompounds, sep=""))
      print(paste(timenow, " : Negative control wells found : ",counternegcntrls, sep=""))
      print(paste(timenow, " : Compound wells found : ",counterskipped, sep=""))
      sink(NULL)

      ## print out to appear in email
      print("------------------------------------------------------------------")
      print(paste(timenow, " : Processed study id : ", idstudy, sep=""))
      print(paste(timenow, " : Study name : ", studyname, sep=""))
      print(paste(timenow, " : Compound wells found : ",countercompounds, sep=""))
      print(paste(timenow, " : Negative control wells found : ",counternegcntrls, sep=""))
      print(paste(timenow, " : Empty wells found : ",counterskipped, sep=""))
    
      # update study status to 'GEN ANALYSIS'
      newstatusname <- "GEN ANALYSIS"
      ussga <- update_study_status(idstudy, newstatusname)
      
      # check for success of the update
      if (ussga == -1) {
        timenow <- format(Sys.time(), "%Y %m %d %X")
        sink(logfile, append=T)
        print(paste(timenow, " : Failed study status update to GEN ANALYSIS for study ID = ", idstudy, sep=""))
        print(paste(timenow, " : Cannot continue, failed to update study status in database.",sep=""))
        sink(NULL)
        print("EVE cherry screen analysis trigger stopped after error updating study status to GEN ANALYSIS. Cannot continue.")
        return(F)
      }

    } # end loop of studies
    
    #Close file
    
  } else {
    # no studies to process
    timenow <- format(Sys.time(), "%Y %m %d %X")
    sink(logfile, append=T)
    print(paste(timenow, " : Number of studies zero for cherry screen ID = ", idcherryscreen, sep=""))
    print(paste(timenow, " : Skipping processing this cherry screen ID.",sep=""))
    sink(NULL)
    return(F)
  } # end check for number of studies

 } # end check for list studies
 
 return(T)
}
