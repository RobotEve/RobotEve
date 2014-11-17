### Contains SQL queries for Eve data access

# Selects mass and cherry screen studies with the correct status that indicates
# they are ready for automatic parameter generation
# Returns: integer list of id_study's
select_eve_studies_rdy_for_paramgen <- function() {
   query <- paste("SELECT s.id_study, ems.id_mass_screen, ecs.id_cherry_screen FROM study s ",
   	 "LEFT OUTER JOIN eve_mass_screen ems ON ems.id_investigation = s.id_investigation ",
	 "AND ems.cycle_number = s.cycle_number ",
	 "LEFT OUTER JOIN eve_cherry_screen ecs ON ecs.id_investigation = s.id_investigation ",
	 "AND ecs.cycle_number = s.cycle_number ",
	 "WHERE s.id_study_status = (SELECT id_study_status FROM study_status WHERE display_desc = 'RUN COMPLETED') ",
	 "HAVING ems.id_mass_screen > 0 OR ecs.id_cherry_screen > 0 ",
	 "ORDER BY s.id_study", 
         sep = "")
   avail_studies <- tryCatch(dbGetQuery(con, statement=query), error = function(err) errorHandler(err, query, "Error on selecting available Eve studies for parameter generation"))
   return(avail_studies)
}

# Selects whether the specified study is in the correct status for processing
# This is a validation to cope with the issue of multiple instances of the parameter generation
# code being run by multiple cron job instances.
# Returns: integer indicating true or false (count = 0 if not in right status, 1 if Ok, -1 if error)
select_study_status_ready_to_process <- function(studyid) {
   query <- paste("SELECT COUNT(id_study) FROM study WHERE id_study = ",studyid, " AND id_study_status = (SELECT id_study_status FROM study_status WHERE display_desc = 'RUN COMPLETED')",
         sep = "")
   errstring <- paste("Error in parameter generation on checking study ready to process for study id = ", studyid, sep="")
   count <- tryCatch(dbGetQuery(con, statement=query), error = function(err) errorHandler(err, query, errstring))
   return(count)
}

# Selects the assay plate instance id equating to a specified study id
# Returns: integer id_plate_instance
select_eve_plate_instance_for_study_id <- function(studyid) {
   query <- paste("SELECT pi.id_plate_instance FROM eve_assay_plate_layout eapl, plate_instance pi ",
         "WHERE eapl.id_study = ", studyid, " AND eapl.id_plate_layout = pi.id_plate_layout ",
         "ORDER BY pi.id_plate_instance DESC LIMIT 1",
         sep = "")
   errstring <- paste("Error in parameter generation on selecting plate instance for study id = ", studyid, sep="")
   plateInstance <- tryCatch(dbGetQuery(con, statement=query), error = function(err) errorHandler(err, query, errstring))
   return(plateInstance)
}

# Selects the investigation name for a study id
# Returns: string investigation name
select_investname_for_study_id <- function(studyid) {
   query <- paste("SELECT i.display_name FROM investigation i, study s ",
         "WHERE s.id_study = ", studyid, " AND s.id_investigation = i.id_investigation", 
         sep = "")
   errstring <- paste("Error in parameter generation on selecting investigation name for study id = ", studyid, sep="")
   investname <- tryCatch(dbGetQuery(con, statement=query), error = function(err) errorHandler(err, query, errstring))
   return(investname)
}

# Selects study name for a study id
# Returns: string study name
select_studyname_for_study_id <- function(studyid) {
   query <- paste("SELECT name FROM study WHERE id_study = ", studyid, sep = "")
   errstring <- paste("Error in parameter generation on selecting study name for study id = ", studyid, sep="")
   studyname <- tryCatch(dbGetQuery(con, statement=query), error = function(err) errorHandler(err, query, errstring))
   return(studyname)
}

# Selects assay plate barcode for a plate instance
# Returns: string assay plate barcode
select_assay_plate_barcode_for_plate_instance <- function(plateInstance) {
   query <- paste("SELECT plate_barcode FROM plate_instance WHERE id_plate_instance = ", plateInstance, sep = "")
   errstring <- paste("Error in parameter generation on selecting plate barcode for plate instance = ", plateInstance, sep="")
   assayplatebc <- tryCatch(dbGetQuery(con, statement=query), error = function(err) errorHandler(err, query, errstring))
   return(assayplatebc)
}

# Selects the filter names used to read a specific plate instance.
# Returns: string list of filternames
select_eve_filternames <- function(plateInstance) {
   query <- paste("SELECT DISTINCT(testname) FROM eve_plate_reading ",
         "WHERE id_plate_instance=", plateInstance, sep = "")
   errstring <- paste("Error in parameter generation on selecting filter names for plate instance = ", plateInstance, sep="")
   filterNames <- tryCatch(dbGetQuery(con, statement=query), error = function(err) errorHandler(err, query, errstring))    
   return(filterNames)
}

# Selects wells with readings for a specific plate_instance and filtername
# Returns: list of wells
select_eve_wells_with_rdgs_for_filter <- function(plateInstance, filtername) {
  quoted_filtername_string <- paste("'",filtername,"'",sep="")
  query <- paste("SELECT DISTINCT epwr.well_row, epwr.well_col ",
        "FROM eve_plate_well_reading epwr, eve_plate_reading epr ",
        "WHERE epwr.id_plate_instance=",plateInstance,
        " AND epr.testname=",quoted_filtername_string,
        " AND epwr.id_plate_instance = epr.id_plate_instance ",
        "AND epwr.read_type = epr.read_type ",
        "AND epwr.read_ts = epr.read_ts ",
        "ORDER BY epwr.well_row,epwr.well_col",
        sep="")
  errstring <- paste("Error in parameter generation on selecting all the well ids for a particular plate instance = ", plateInstance, " and filtername", sep="")
  wellslist <- tryCatch(dbGetQuery(con, statement=query), error = function(err) errorHandler(err, query, errstring)) 
  return(wellslist)
}

# Selects readings for a specific plate instance, well and filtername
# Returns: list of timestamps and readings
# N.B. might need to use reading_2 if we ever have fluorescence polarisation readings
select_eve_well_readings <- function(plateInstance, filtername, wellrow, wellcol) {
  quoted_filtername <- paste("'",filtername,"'",sep="")
  quoted_well_row <- paste("'",wellrow,"'",sep="")
  query <- paste("SELECT epwr.read_ts, epwr.reading_1 ",
        "FROM eve_plate_well_reading epwr, eve_plate_reading epr ",
        "WHERE epwr.id_plate_instance=", plateInstance,
        " AND epwr.well_row=", quoted_well_row,
        " AND epwr.well_col=", wellcol,
        " AND epr.testname=", quoted_filtername,
        " AND epwr.id_plate_instance = epr.id_plate_instance ",
        "AND epwr.read_type = epr.read_type ",
        "AND epwr.read_ts = epr.read_ts ",
        "ORDER BY epwr.read_ts",
        sep = "")
  errstring <- paste("Error in parameter generation on selecting readings for a particular plate instance = ", plateInstance, " filtername = ", filtername, " well row = ", wellrow, " and col = ", wellcol, sep="")
  rdgslist <- tryCatch(dbGetQuery(con, statement=query), error = function(err) errorHandler(err, query, errstring)) 
  return(rdgslist)
}

# Insert a failure row to the growth parameters table for a specific plate instance, well and filtername
insert_eve_failure <- function(plateInstance, wellrow, wellcol, filtername){
  insquery <- paste("INSERT INTO eve_assay_well_growth_parameters",
           " (id_plate_instance, well_col, well_row, testname, insert_ts",
           ", code_version, hadError) ",
           " VALUES(", plateInstance,
           ", ", wellcol,
           ", ", paste("'",wellrow,"'",sep=""),
           ", ", paste("'",filtername,"'",sep=""),
           ", now(), ", params_code_version,
           ", 1)", sep="")
  errstring <- paste("Error in parameter generation on inserting a failed row for plate instance = ", plateInstance, " filtername = ", filtername, " well row = ", wellrow, " and col = ", wellcol, " and set of timestamps", sep="")
  result = try(dbSendQuery(con, statement=insquery))
  if(inherits(result,"try-error")) {
    timenow <- format(Sys.time(), "%Y %m %d %X")
    sink(logfile, append=T)
    print("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^")
    print("Error message : ")
    print (errstring)
    print(paste("SQL query : ", insquery, sep=""))
    print("vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv")
    sink(NULL)
    return(-1)
  } else {
    return(1)
  }
}

# Insert a successful row to the growth parameters table for a specific plate instance, well, filtername
# and set of parameters
insert_eve_query <- function(output, plateInstance, wellrow, wellcol, filtername){
  insquery <- paste("INSERT INTO eve_assay_well_growth_parameters (id_plate_instance, well_col, well_row, testname, insert_ts, code_version, hadError, startvalue, startvaluetime, minvalue, minvaluetime, maxvalue, maxvaluetime, endvalue, endvaluetime, lagtime, miylagtime, startlinear, endlinear, linearslope, doubletime, durlinear, snratio)",
        " VALUES(", plateInstance,
        ", ", wellcol,
        ", ", paste("'",wellrow,"'",sep=""),
        ", ", paste("'",filtername,"'",sep=""),
        ", now(), ", params_code_version,
        ", 0",
        ", ", output$StartValue,
        ", ", output$StartValueTime,
        ", ", output$MinValue,
        ", ", output$MinValueTime,
        ", ", output$MaxValue,
        ", ", output$MaxValueTime,
        ", ", output$EndValue,
        ", ", output$EndValueTime,
        ", ", output$lagtime,
        ", ", output$miylagtime,
        ", ", output$startlinear,
        ", ", output$endlinear,
        ", ", output$linearslope,
        ", ", output$doubletime,
        ", ", output$dur_linear,
        ", ", output$snratio,
        ")", sep="")
  errstring <- paste("Error in parameter generation on inserting parameters for plate instance = ", plateInstance, " filtername = ", filtername, " well row = ", wellrow, " and col = ", wellcol, " and set of parameters", sep="")
  result <- try(dbSendQuery(con, statement=insquery))
  if(inherits(result,"try-error")) {
    timenow <- format(Sys.time(), "%Y %m %d %X")
    sink(logfile, append=T)
    print("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^")
    print("Error message : ")
    print (errstring)
    print(paste("SQL query : ", insquery, sep=""))
    print("vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv")
    sink(NULL)
    return(-1)
  } else {    
    return(1)
  }
}

# Update study status to a new status by name
update_study_status <- function(studyid, newstatusname) {
  quoted_statusname <- paste("'",newstatusname,"'",sep="")
  updquery <- paste("UPDATE study SET id_study_status = (SELECT id_study_status FROM study_status ",
           "WHERE display_desc=", quoted_statusname,
           ") WHERE id_study=", studyid,
           sep="")
  errstring <- paste("Error in parameter generation on study status for study id = ", studyid, " and new status name = ", newstatusname, sep="")
  result <- try(dbSendQuery(con, statement=updquery))
  if(inherits(result,"try-error")) {
    timenow <- format(Sys.time(), "%Y %m %d %X")
    sink(logfile, append=T)
    print("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^")
    print("Error message : ")
    print (errstring)
    print(paste("SQL query : ", updquery, sep=""))
    print("vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv")
    sink(NULL)
    return(-1)
  } else {
    return(1)
  }  
}

# Error handler writes the SQL message to the logfile
errorHandler <- function(err, query, strMsg) {
  timenow <- format(Sys.time(), "%Y %m %d %X")
  sink(logfile, append=T)
  print("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^")
  print("Error message passed to handler : ")
  print (strMsg)
  print(paste("SQL query : ", query, sep=""))
  if(!is.null(err)) {
      print("Exception error message : ")
      print(err)
  }
  print("vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv")
  sink(NULL)
  return(-1)
}
