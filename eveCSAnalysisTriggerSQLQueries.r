### Contains SQL queries for Eve data access

# Selects cherry screen cycles with counts of studies ready for analysis
# Returns: integer list of id_cherry_screens
select_eve_cs_cycles_rdy_for_analysis <- function() {
   query <- paste("SELECT cs.id_cherry_screen, t1.count_params, t2.count_unfinished ",
                  "FROM eve_cherry_screen cs ",
                  "LEFT OUTER JOIN (SELECT ecs.id_cherry_screen, count(s.id_study) AS count_params ",
                  "FROM eve_cherry_screen ecs, study s ",
                  "WHERE s.id_study_status = (SELECT id_study_status FROM study_status WHERE display_desc IN('PARAMETERS')) ",
                  "AND s.id_investigation = ecs.id_investigation ",
                  "AND s.cycle_number = ecs.cycle_number ",
                  "GROUP BY ecs.id_cherry_screen ",
                  "ORDER BY ecs.id_cherry_screen) t1 ON cs.id_cherry_screen = t1.id_cherry_screen ",
                  "LEFT OUTER JOIN (SELECT ecs.id_cherry_screen, count(s.id_study) AS count_unfinished ",
                  "FROM eve_cherry_screen ecs, study s ",
                  "WHERE s.id_study_status IN(SELECT id_study_status FROM study_status WHERE display_desc IN('RUNNING','RUN COMPLETED','GEN PARAMETERS')) ",
                  "AND s.id_investigation = ecs.id_investigation ",
                  "AND s.cycle_number = ecs.cycle_number ",
                  "GROUP BY ecs.id_cherry_screen ",
                  "ORDER BY ecs.id_cherry_screen) t2 ON cs.id_cherry_screen = t2.id_cherry_screen ",
                  "WHERE t1.count_params IS NOT NULL AND t1.count_params > 0 AND t2.count_unfinished IS NULL ",
                  "ORDER BY cs.id_cherry_screen", 
         sep = "")
   avail_cherry_scrn_cycles <- tryCatch(dbGetQuery(con, statement=query), error = function(err) errorHandler(err, query, "Error on selecting Eve cherry screen cycles available for triggered analysis"))
   return(avail_cherry_scrn_cycles)
}

# Selects studies for a cherry screen cycle ready for analysis
# Returns: integer list of id_study
select_studies_for_cherry_screen<- function(idcherryscreen) {
   query <- paste("SELECT s.id_study ",
                  "FROM eve_cherry_screen ecs, study s ",
                  "WHERE s.id_study_status IN(SELECT id_study_status FROM study_status WHERE display_desc IN('PARAMETERS','GEN ANALYSIS','ANALYSED')) ",
                  "AND s.id_investigation = ecs.id_investigation ",
                  "AND s.cycle_number = ecs.cycle_number ",
                  "AND id_cherry_screen = ",idcherryscreen,
                  " ORDER BY s.id_study", 
         sep = "")
   avail_studies <- tryCatch(dbGetQuery(con, statement=query), error = function(err) errorHandler(err, query, "Error in analysis trigger on selecting studies for a cherry screen"))
   return(avail_studies)
}

# Selects the investigation name for a cherry screen id
# Returns: string investigation name
select_investname_for_cherry_screen_id <- function(idcherryscreen) {
   query <- paste("SELECT i.display_name FROM investigation i, eve_cherry_screen ecs ",
                  "WHERE ecs.id_cherry_screen = ",idcherryscreen,
                  " AND ecs.id_investigation = i.id_investigation ", 
                  sep = "")
   errstring <- paste("Error in analysis trigger on selecting investigation name for cherry screen id = ", idcherryscreen, sep="")
   investname <- tryCatch(dbGetQuery(con, statement=query), error = function(err) errorHandler(err, query, errstring))
   return(investname)
}

# Selects study name for a study id
# Returns: string study name
select_studyname_for_study_id <- function(idstudy) {
   query <- paste("SELECT name FROM study WHERE id_study = ", idstudy, sep = "")
   errstring <- paste("Error in analysis trigger on selecting study name for study id = ", idstudy, sep="")
   studyname <- tryCatch(dbGetQuery(con, statement=query), error = function(err) errorHandler(err, query, errstring))
   return(studyname)
}

# Selects assay plate barcode for a plate instance
# Returns: string assay plate barcode
select_assay_plate_barcode_for_plate_instance <- function(plateInstance) {
   query <- paste("SELECT plate_barcode FROM plate_instance WHERE id_plate_instance = ", plateInstance, sep = "")
   errstring <- paste("Error in analysis trigger on selecting plate barcode for plate instance = ", plateInstance, sep="")
   assayplatebc <- tryCatch(dbGetQuery(con, statement=query), error = function(err) errorHandler(err, query, errstring))
   return(assayplatebc)
}

# Selects investigation id for a cherry screen ID
# Returns: integer id_investigation
select_invest_id_for_cherry_screen<- function(idcherryscreen) {
   query <- paste("SELECT ecs.id_investigation ",
                  "FROM eve_cherry_screen ecs ",
                  "WHERE id_cherry_screen = ",idcherryscreen,
                  sep = "")
   idinvestigation<- tryCatch(dbGetQuery(con, statement=query), error = function(err) errorHandler(err, query, "Error on selecting investigation ID for a cherry screen"))
   return(idinvestigation)
}

# Selects cycle number for a cherry screen ID
# Returns: integer cycle_number
select_cycle_number_for_cherry_screen<- function(idcherryscreen) {
   query <- paste("SELECT ecs.cycle_number ",
                  "FROM eve_cherry_screen ecs ",
                  "WHERE id_cherry_screen = ",idcherryscreen,
                  sep = "")
   cycle_number<- tryCatch(dbGetQuery(con, statement=query), error = function(err) errorHandler(err, query, "Error on selecting cycle number for a cherry screen"))
   return(cycle_number)
}

# Selects list of wells for a study
# Returns: list of well ids by plate instance, row and column
# NB subselect gets highest plate instance id only if there is more than one (eg after error)
select_assay_wells_list_for_study <- function(idstudy) {
   query <- paste("SELECT eaw.id_plate_instance, eaw.well_row, eaw.well_col ", 
                  "FROM eve_assay_well eaw, (SELECT pi.id_plate_instance ", 
                  "FROM eve_assay_plate_layout epl, plate_instance pi ", 
                  "WHERE epl.id_study = ",idstudy,
                  " AND epl.id_plate_layout = pi.id_plate_layout ", 
                  "ORDER BY pi.id_plate_instance DESC LIMIT 1) AS temp1 ", 
                  "WHERE eaw.id_plate_instance = temp1.id_plate_instance ", 
                  "ORDER BY eaw.id_plate_instance, eaw.well_row, eaw.well_col ", 
         sep = "")
   avail_wells <- tryCatch(dbGetQuery(con, statement=query), error = function(err) errorHandler(err, query, "Error on selecting available wells for a study"))
   return(avail_wells)
}

# Selects compound details and parameters for an assay well if available
# Returns: csv string representation of details
select_compound_data_for_assay_well <- function(idplateinstance,wellrow,wellcol) {
  quoted_well_row <- paste("'",wellrow,"'",sep="")
  query <- paste("SELECT eaw.id_plate_instance, eaw.well_row, eaw.well_col, ecsm.id_mixture, c.id_chemical, c.name, c.smiles, egpchy.testname, egpchy.hadError, ",
                 "egpchy.startvalue, egpchy.endvalue, egpchy.miylagtime, egpchy.doubletime, egpsapph.testname, egpsapph.hadError, egpsapph.startvalue, ",
                 "egpsapph.endvalue, egpsapph.miylagtime, egpsapph.doubletime, egpven.testname, egpven.hadError, egpven.startvalue, egpven.endvalue, ",
                 "egpven.miylagtime, egpven.doubletime, pi.id_plate_layout, eapl.id_study, psfcp.protocol_amount AS vol_cmpd_nl, ((psfcp.protocol_amount/50000) * 10000) AS cmpd_conc_um ",
                 "FROM eve_assay_well eaw ",
                 "LEFT OUTER JOIN eve_compound_library_well_to_assay_well_transfer eawt ON eawt.dest_id_plate_instance = eaw.id_plate_instance ",
                 "AND eawt.dest_well_row = eaw.well_row AND eawt.dest_well_col = eaw.well_col ",
                 "LEFT OUTER JOIN eve_compound_library_well_stock_mixture ecsm ON ecsm.id_plate_instance = eawt.source_id_plate_instance ",
                 "AND ecsm.well_row = eawt.source_well_row AND ecsm.well_col = eawt.source_well_col ",
                 "LEFT OUTER JOIN stock_mixture_used_item smui ON ecsm.id_mixture = smui.id_mixture ",
                 "LEFT OUTER JOIN supplier_item si ON smui.id_supplier_item = si.id_supplier_item ",
                 "LEFT OUTER JOIN chemical c ON si.id_product = c.id_chemical ",
                 "LEFT OUTER JOIN eve_assay_well_growth_parameters egpchy ON egpchy.id_plate_instance = eaw.id_plate_instance AND egpchy.well_row = eaw.well_row AND egpchy.well_col = eaw.well_col AND egpchy.testname = 'CHERRY580X384' ", 
                 "LEFT OUTER JOIN eve_assay_well_growth_parameters egpsapph ON egpsapph.id_plate_instance = eaw.id_plate_instance AND egpsapph.well_row = eaw.well_row AND egpsapph.well_col = eaw.well_col AND egpsapph.testname = 'SAPPHIRE405X384' ",
                 "LEFT OUTER JOIN eve_assay_well_growth_parameters egpven ON egpven.id_plate_instance = eaw.id_plate_instance AND egpven.well_row = eaw.well_row AND egpven.well_col = eaw.well_col AND egpven.testname = 'VENUS500X384' ",
                 "LEFT OUTER JOIN plate_instance pi ON pi.id_plate_instance = eaw.id_plate_instance ",
                 "LEFT OUTER JOIN eve_assay_plate_layout eapl ON eapl.id_plate_layout = pi.id_plate_layout ",
                 "LEFT OUTER JOIN study s ON s.id_study = eapl.id_study ",
                 "LEFT OUTER JOIN cycle_of_investigation_plate_schema coips ON coips.id_investigation = s.id_investigation AND coips.cycle_number = s.cycle_number ",
                 "LEFT OUTER JOIN plate_schema_well_content pswc ON pswc.id_plate_schema = coips.id_plate_schema AND pswc.well_row = eaw.well_row AND pswc.well_col = eaw.well_col AND pswc.factor_number = 2 ",
                 "LEFT OUTER JOIN plate_schema_factor_component_protocol psfcp ON psfcp.id_plate_schema = pswc.id_plate_schema AND psfcp.factor_number = pswc.factor_number AND psfcp.component_number = pswc.component_number AND psfcp.protocol_number = 1 ",
                 "WHERE eaw.id_plate_instance = ",idplateinstance,
                 " AND eaw.well_row = ",quoted_well_row,
                 " AND eaw.well_col = ",wellcol,
                 " AND c.id_chemical != 1313 ",
                 "ORDER BY c.id_chemical", 
         sep = "")
   compounddata <- tryCatch(dbGetQuery(con, statement=query), error = function(err) errorHandler(err, query, "Error on selecting content and parameters for a compound assay well"))
   return(compounddata)
}

# Selects negative control details and parameters for an assay well if available
# Returns: csv string representation of details
select_neg_cntrl_data_for_assay_well <- function(idplateinstance,wellrow,wellcol) {
  quoted_well_row <- paste("'",wellrow,"'",sep="")
  query <- paste("SELECT eaw.id_plate_instance, eaw.well_row, eaw.well_col, ecsm.id_mixture, c.id_chemical, c.name, c.smiles, egpchy.testname, egpchy.hadError, ",
                 "egpchy.startvalue, egpchy.endvalue, egpchy.miylagtime, egpchy.doubletime, egpsapph.testname, egpsapph.hadError, egpsapph.startvalue, ",
                 "egpsapph.endvalue, egpsapph.miylagtime, egpsapph.doubletime, egpven.testname, egpven.hadError, egpven.startvalue, egpven.endvalue, ",
                 "egpven.miylagtime, egpven.doubletime, pi.id_plate_layout, eapl.id_study, psfcp.protocol_amount AS vol_cmpd_nl, ((psfcp.protocol_amount/50000) * 10000) AS cmpd_conc_um ",
                 "FROM eve_assay_well eaw ",
                 "LEFT OUTER JOIN eve_compound_library_well_to_assay_well_transfer eawt ON eawt.dest_id_plate_instance = eaw.id_plate_instance ",
                 "AND eawt.dest_well_row = eaw.well_row AND eawt.dest_well_col = eaw.well_col ",
                 "LEFT OUTER JOIN eve_compound_library_well_stock_mixture ecsm ON ecsm.id_plate_instance = eawt.source_id_plate_instance ",
                 "AND ecsm.well_row = eawt.source_well_row AND ecsm.well_col = eawt.source_well_col ",
                 "LEFT OUTER JOIN stock_mixture_used_item smui ON ecsm.id_mixture = smui.id_mixture ",
                 "LEFT OUTER JOIN supplier_item si ON smui.id_supplier_item = si.id_supplier_item ",
                 "LEFT OUTER JOIN chemical c ON si.id_product = c.id_chemical ",
                 "LEFT OUTER JOIN eve_assay_well_growth_parameters egpchy ON egpchy.id_plate_instance = eaw.id_plate_instance AND egpchy.well_row = eaw.well_row AND egpchy.well_col = eaw.well_col AND egpchy.testname = 'CHERRY580X384' ", 
                 "LEFT OUTER JOIN eve_assay_well_growth_parameters egpsapph ON egpsapph.id_plate_instance = eaw.id_plate_instance AND egpsapph.well_row = eaw.well_row AND egpsapph.well_col = eaw.well_col AND egpsapph.testname = 'SAPPHIRE405X384' ",
                 "LEFT OUTER JOIN eve_assay_well_growth_parameters egpven ON egpven.id_plate_instance = eaw.id_plate_instance AND egpven.well_row = eaw.well_row AND egpven.well_col = eaw.well_col AND egpven.testname = 'VENUS500X384' ",
                 "LEFT OUTER JOIN plate_instance pi ON pi.id_plate_instance = eaw.id_plate_instance ",
                 "LEFT OUTER JOIN eve_assay_plate_layout eapl ON eapl.id_plate_layout = pi.id_plate_layout ",
                 "LEFT OUTER JOIN study s ON s.id_study = eapl.id_study ",
                 "LEFT OUTER JOIN cycle_of_investigation_plate_schema coips ON coips.id_investigation = s.id_investigation AND coips.cycle_number = s.cycle_number ",
                 "LEFT OUTER JOIN plate_schema_well_content pswc ON pswc.id_plate_schema = coips.id_plate_schema AND pswc.well_row = eaw.well_row AND pswc.well_col = eaw.well_col AND pswc.factor_number = 2 ",
                 "LEFT OUTER JOIN plate_schema_factor_component_protocol psfcp ON psfcp.id_plate_schema = pswc.id_plate_schema AND psfcp.factor_number = pswc.factor_number AND psfcp.component_number = pswc.component_number AND psfcp.protocol_number = 1 ",
                 "WHERE eaw.id_plate_instance = ",idplateinstance,
                 " AND eaw.well_row = ",quoted_well_row,
                 " AND eaw.well_col = ",wellcol,
                 " HAVING cmpd_conc_um = 0 ",
                 "ORDER BY c.id_chemical", 
         sep = "")
   compounddata <- tryCatch(dbGetQuery(con, statement=query), error = function(err) errorHandler(err, query, "Error on selecting content and parameters for a negative control compound assay well"))
   return(compounddata)
}

# Update study status to a new status by name
update_study_status <- function(idstudy, newstatusname) {
  quoted_statusname <- paste("'",newstatusname,"'",sep="")
  updquery <- paste("UPDATE study SET id_study_status = (SELECT id_study_status FROM study_status ",
           "WHERE display_desc=", quoted_statusname,
           ") WHERE id_study=", idstudy,
           sep="")
  errstring <- paste("Error in cherry screen analysis trigger on study status for study id = ", idstudy, " and new status name = ", newstatusname, sep="")
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
