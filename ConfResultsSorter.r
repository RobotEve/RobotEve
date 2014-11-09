################################################################################
#
# Program written in R
# Kevin Williams
# Test version, 20 Oct 2011
#
################################################################################
#
# Sorting confirmation screen results for use in EveActivity.r
#
################################################################################
#
# Input: a single .csv file of 28 columns for 3 strains; this will contain
#      well/plate location, compound info and parameters for start fluorescence,
#      growth, lagtime & doubling time.
#
# Main outputs: (1) a new csv file, saved as "screen_results.csv" (25 columns) 
#      for use in EveActivity.r, that contains multiple index numbers in place
#      of each iterated compid/concentration combination.
#      (2) an index file containing the new index versus the compid/conc data
#
#
# Column names (not named in input or output files):
#   A/1  id_plate_instance   : numeric, plate number
#   B/2  well_row            : letter, always A-P
#   C/3  well_col            : numeric, always 1-24
#   D/4  id_mixture          : string, two entries for each filled well, one
#                          entry for each negative control, no entries for empty
#                          wells, uses library name/code for substances on test
#   E/5  id_chemical         : numeric, Eve ID for chemicals in well, two 
#                          entries for compounds (compound+DMSO), one entry for
#                          negative controls
#   F/6  name                : string, full compound name (or DMSO)
#   G/7  smiles              : SMILES code for F
#   H/8  cherry_testname     : alphanumeric
#   I/9  cherry_hadError     : numeric, should be zero
#   J/10  cherry_startvalue  : numeric, initial fluorescence
#   K/11  cherry_endvalue    : numeric, final fluorescence
#   L/12  cherry_miylagtime  : numeric
#   M/13  cherry_doubletime  : numeric
#   N/14  sapphire_testname  : alphanumeric
#   O/15  sapphire_hadError  : numeric, should be zero
#   P/16  sapphire_startvalue: numeric, initial fluorescence
#   Q/17  sapphire_endvalue  : numeric, final fluorescence
#   R/18  sapphire_miylagtime: numeric
#   S/19  sapphire_doubletime: numeric
#   T/20  venus_testname     : alphanumeric
#   U/21  venus_hadError     : numeric, should be zero
#   V/22  venus_startvalue   : numeric, initial fluorescence
#   W/23  venus_endvalue     : numeric, final fluorescence
#   X/24  venus_miylagtime   : numeric
#   Y/25  venus_doubletime   : numeric
#   Z/26  id_study1          : numeric
#  AA/27  id_study2          : numeric
#  AB/28  vol_cmpd_nl        : numeric, for confirmation screen use
#  AC/29  cmpd_conc_um       : numeric, for confirmation screen use
#
################################################################################
#
# 
#
################################################################################
#
#
rm(list=ls(all=TRUE))
# removes all previous variables, preventing cross-contamination

start_time <- format(Sys.time(),"%Y%m%d%H%M%S")
 


filename <- "confirmation_screen_results.csv"
confirmation_data <- read.csv(filename)
colnames(confirmation_data) <- c("id_plate_instance","well_row","well_col","id_mixture",
    "id_chemical","name","smiles","cherry_testname","cherry_hadError",
    "cherry_startvalue","cherry_endvalue","cherry_miylagtime","cherry_doubletime",
    "sapphire_testname","sapphire_hadError","sapphire_startvalue",
    "sapphire_endvalue","sapphire_miylagtime","sapphire_doubletime",
    "venus_testname","venus_hadError","venus_startvalue","venus_endvalue",
    "venus_miylagtime","venus_doubletime","id_study1","id_study2","vol_cmpd_nl",
    "cmpd_conc_um")
# puts headings on all columns
    
confirmation_sorted <- confirmation_data[order(confirmation_data$id_chemical,
    confirmation_data$cmpd_conc_um),]
# sorts by id_chemical as primary, and concentration as secondary    

new_index <- 0
m <- dim(confirmation_sorted)[1]
confirmation_indexed <- cbind(confirmation_sorted,new_index)
n <- 30001
for (i in 1:m){
    if ((confirmation_indexed)[i,5]==1313){
      confirmation_indexed[i,30] <- 1313}
      else {
      confirmation_indexed[i,30] <- n
      n <- n+1}    
  }

# puts a new index label to all compounds (keeping DMSO/1313 the same)

indexed_columns <- cbind(confirmation_indexed$id_chemical,
      confirmation_indexed$cmpd_conc_um, confirmation_indexed$new_index)
indexed_compounds <- subset(indexed_columns,!(indexed_columns[,3] == 1313))

# removes all DMSO lines for the indexed list
    
reindexed_results <- confirmation_indexed
reindexed_results$id_chemical <- reindexed_results$new_index
reindexed_results$id_study1 <- NULL
reindexed_results$id_study2 <- NULL
reindexed_results$vol_cmpd_nl <- NULL
reindexed_results$cmpd_conc_um <- NULL
reindexed_results$new_index <- NULL
#removes confirmation-specific data to enable file to fit into EveActivity.r

#write.table(indexed_compounds,file="indexed_compounds.csv",sep=",",col.names=FALSE,row.names=FALSE)
write.table(indexed_compounds,file="indexed_compounds.csv",sep=",",col.names=TRUE,row.names=FALSE)
write.table(reindexed_results,file="screen_results.csv",sep=",",col.names=TRUE,row.names=FALSE)

#write.csv(s_hit_compounds,file="sapphire_hits.csv")

#activity_data$X <- NULL 
#colnames(activity_data)[1] <- "plate"
#colnames(activity_data)[2] <- "well_row"
#colnames(activity_data)[3] <- "well_col"
#colnames(activity_data)[5] <- "id_chemical"
#colnames(activity_data)[6] <- "name"
#colnames(activity_data)[7] <- "smiles"
#c_ratio <- activity_data$c_ratio
#m <- dim(activity_data)[1]
## gives the depth of the full data set to m
#a <- 0
#c_fluor_tag <- array(1:m)
#c_score <- array(1:m)
#s_fluor_tag <- array(1:m)
#s_score <- array(1:m)
#v_fluor_tag <- array(1:m)
#v_score <- array(1:m)
#total_score <- array(1:m)
#fluor_score <- array(1:m)
#ratio_total <- array(1:m)
#c_tot_ratio <- array(1:m)
#s_tot_ratio <- array(1:m)
#v_tot_ratio <- array(1:m)
#c_s_ratio <- array(1:m)
#c_v_ratio <- array(1:m)
#s_c_ratio <- array(1:m)
#s_v_ratio <- array(1:m)
#v_c_ratio <- array(1:m)
#v_s_ratio <- array(1:m)


#for (n in 1:m){
#  c_fluor_tag[n] <- 0
 # c_score[n] <- 0
  #s_fluor_tag[n] <- 0
#  s_score[n] <- 0
 # v_fluor_tag[n] <- 0
  #v_score[n] <- 0
  
#  ratio_total[n] <- activity_data$c_ratio[n] + activity_data$s_ratio[n] + 
 #                   activity_data$v_ratio[n]
#  c_tot_ratio[n] <- activity_data$c_ratio[n]/ratio_total[n]
 # s_tot_ratio[n] <- activity_data$s_ratio[n]/ratio_total[n]
  #v_tot_ratio[n] <- activity_data$v_ratio[n]/ratio_total[n]
#  c_s_ratio[n] <-  activity_data$c_ratio[n]/activity_data$s_ratio[n]
 # c_v_ratio[n] <-  activity_data$c_ratio[n]/activity_data$v_ratio[n]
  #s_c_ratio[n] <-  activity_data$s_ratio[n]/activity_data$c_ratio[n]
#  s_v_ratio[n] <-  activity_data$s_ratio[n]/activity_data$v_ratio[n]
 # v_c_ratio[n] <-  activity_data$v_ratio[n]/activity_data$c_ratio[n]
  #v_s_ratio[n] <-  activity_data$v_ratio[n]/activity_data$s_ratio[n]
  
  
#  if (activity_data$c_fluor[n] > 0.08)
 #   {c_fluor_tag[n] <- 1}
  #if (activity_data$s_fluor[n] > 0.08)
   # {s_fluor_tag[n] <- 1
    #a <- a+1}
#  if (activity_data$v_fluor[n] > 0.08)
 #   {v_fluor_tag[n] <- 1}
## above if statements check whether compound is fluoro for any of the filters
#  fluor_score[n] <-  c_fluor_tag[n] + s_fluor_tag[n] + v_fluor_tag[n]
 # if (fluor_score[n] < 1)
# if compound isn't fluoro... give it an activity score (2=hit, 1=PossActive):
#    {if (activity_data$c_ratio[n] < 0.8)
 #       {c_score[n] <- 2}
  #      else
   #     {if (activity_data$c_miylagtime[n] > 4)
    #        {c_score[n] <- c_score[n] + 1}
     #   if (activity_data$c_doubletime[n] > 1.5)
      #      {c_score[n] <- c_score[n] + 1}
       #     }
#    if (activity_data$s_ratio[n] < 0.8)
 #       {s_score[n] <- 2}
  #      else
   #     {if (activity_data$s_miylagtime[n] > 4)
    #        {s_score[n] <- s_score[n] + 1}
     #   if (activity_data$s_doubletime[n] > 1.5)
      #      {s_score[n] <- s_score[n] + 1}
       #     }
#    if (activity_data$v_ratio[n] < 0.8)
 #       {v_score[n] <- 2}
  #      else
   #     {if (activity_data$v_miylagtime[n] > 4)
    #        {v_score[n] <- v_score[n] + 1}
     #   if (activity_data$v_doubletime[n] > 1.5)
      #      {v_score[n] <- v_score[n] + 1}
       #     }
        #}
#    total_score[n] <- c_score[n] + s_score[n] + v_score[n]
 #   }
  #  
#fluor_score <- as.data.frame(fluor_score)
#total_score <- as.data.frame(total_score)
#c_score <- as.data.frame(c_score)
#s_score <- as.data.frame(s_score)
#v_score <- as.data.frame(v_score)

#ratio_total <- as.data.frame(ratio_total)
#c_tot_ratio <- as.data.frame(c_tot_ratio)
#s_tot_ratio <- as.data.frame(s_tot_ratio)
#v_tot_ratio <- as.data.frame(v_tot_ratio)

#c_s_ratio <- as.data.frame(c_s_ratio)
#c_v_ratio <- as.data.frame(c_v_ratio)
#s_c_ratio <- as.data.frame(s_c_ratio)
#s_v_ratio <- as.data.frame(s_v_ratio)
#v_c_ratio <- as.data.frame(v_c_ratio)
#v_s_ratio <- as.data.frame(v_s_ratio)

#analysed_data <- data.frame(activity_data$plate,activity_data$well_row,
 # activity_data$well_col,activity_data$compid,activity_data$id_chemical,
  #activity_data$name,activity_data$smiles,activity_data$c_ratio,
#  activity_data$s_ratio,activity_data$v_ratio,fluor_score,total_score,
 # c_score,s_score,v_score,ratio_total,c_tot_ratio,s_tot_ratio,v_tot_ratio,
  #c_s_ratio,c_v_ratio,s_c_ratio,s_v_ratio,v_c_ratio,v_s_ratio)

## sort data
#analysed_data_fluoro <- analysed_data[order(-fluor_score),]
## gives data frame with high fluoro scores at top of list
#analysed_data_toxic <- analysed_data[order(fluor_score,-total_score),]
## gives data frame with high fluoro at bottom of list, then sorts remainder
##   to give high total score (try and use DefTox=6,ProbTox=5) at top
#analysed_data_c_hit <- analysed_data[order(fluor_score,-c_score,total_score),]
#analysed_data_s_hit <- analysed_data[order(fluor_score,-s_score,total_score),]
#analysed_data_v_hit <- analysed_data[order(fluor_score,-v_score,total_score),]
## these give data frames with fluoro at end of list, and sort the respective
##   hits with cleanest at the top, moving through higher total scores until
##   you get the ProbTox & DefTox ones, then all the inactive ones (with fluoro
##   last)

#a <- 0
#for (n in 1:m){
 # if (analysed_data_fluoro[n,11] < 1)
  #  {n <- m}
   # else
    #{a <- a + 1}  
#}
#fluoro_compounds <- analysed_data_fluoro[1:a,]
#write.csv(fluoro_compounds,file="fluoro_compounds.csv")
## generates a list of fluorescent compounds

#a <- 0
#b <- 0
#for (n in 1:m){
#  if (analysed_data_toxic[n,12] < 5)
 #   {n <- m}
  #  else
   # {a <- a + 1}  
#}
#toxic_compounds <- analysed_data_toxic[1:a,]
#write.csv(toxic_compounds,file="toxic_compounds.csv")

#a <- 0
#for (n in 1:m){
#  if (analysed_data_c_hit[n,13] > 1)
 #   {if (analysed_data_c_hit[n,12] < 6)
  #    {a <- a + 1}  
#}   }
#c_hit_compounds <- analysed_data_c_hit[1:a,]
#write.csv(c_hit_compounds,file="cherry_hits.csv")


#a <- 0
#for (n in 1:m){
 # if (analysed_data_s_hit[n,14] > 1)
  #  {if (analysed_data_s_hit[n,12] < 6)
 #     {a <- a + 1}  
#    }
#}
#s_hit_compounds <- analysed_data_s_hit[1:a,]
#write.csv(s_hit_compounds,file="sapphire_hits.csv")


#a <- 0
#for (n in 1:m){
#  if (analysed_data_v_hit[n,15] > 1)
   # {if (analysed_data_v_hit[n,12] < 6)
  #    {a <- a + 1}  
 #   }
#}
#v_hit_compounds <- analysed_data_v_hit[1:a,]
#write.csv(v_hit_compounds,file="venus_hits.csv")
