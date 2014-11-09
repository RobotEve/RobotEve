################################################################################
#
# Program written in R
# Kevin Williams
# Test version, updated 11 March 2011
#
################################################################################
#
# Sorting a batch (or single plate) experiment run by Robot Eve
#
################################################################################
#
# Input: a single .csv file of up to 28 columns for 3 strains (34 columns if we
#      get to 4 strains) and a variable number of rows; this will contain
#      positive & negative controls, compound activity and empty well data.
#
# Main outputs: a single .csv file containing activity measurements for all
#      chromophores for the compounds on test.
#      Individual files needed for Kurt's code
#
# Other outputs: statistics for negative controls, statistics for positive
#      controls, list of empty wells.
#
# Column names:
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
#   Z/26  id_study           : numeric
#  AA/27  vol_cmpd_nl        : numeric, for confirmation screen use
#  AB/28  cmpd_conc_um       : numeric, for confirmation screen use
#
################################################################################
#
# The algorithm
#
#  (i) Remove and record all empty wells
# (ii) Split the dataset into an array for each individual plate, and conduct
#        the following commands on a plate-by-plate basis
#(iii) Split the plate data into positive and negative controls, and sample
#        compounds
# (iv) Calculate the mean and standard deviation for all numeric data columns
#        between J and Y (12 columns)
#  (v) Check hadError columns are all clear (I, O & U)
# (vi) Use the means from (iv) to calculate four values for each chromophore
#        (for each positive control and compound):
#
#                          J  -  meanJnegctrl
#          c_fluor =  ---------------------------
#                     meanKnegctrl - meanJnegctrl
#
#
#                               K  -  J
#          c_rat   =  ---------------------------
#                     meanKnegctrl - meanJnegctrl
#
#
#          c_miy   =  L  - meanLnegctrl
#
#
#                           M
#          c_DT    =  ------------
#                     meanMnegctrl
#
# At this point, we will have the basic ratios for all compounds/controls. There
#   will be a need to recombine all the recalculated data, which can now be
#   compared properly on a plate-by-plate basis.
#
################################################################################
#
# The program
#
################################################################################
#
# Step (i): Remove and record all empty wells
#
# Get file
# Identify all lines with no entry in D
# Remove all these lines into another file
# Save the file of empty wells
# Save the truncated file for step (ii)
#
################################################################################

# a<-read.csv("PGK_TS2_test5.csv")
# c<-a[order(a$id_chem,na.last=FALSE),]


################################################################################
#
# Step (ii): Split dataset into individual plates
#
# Open truncated file
# Make a list of all unique entries in column A
# Sort all lines into a file described by the relevant id_plate_instance
# Save plate files
# Save plate list together with the relevant batch identity (id_study)
#
################################################################################

filename <- "screen_results.csv"                                         #
all_data <- read.csv(filename)  
colnames(all_data) <- c("id_plate_instance","well_row","well_col","id_mixture",
    "id_chemical","name","smiles","cherry_testname","cherry_hadError",
    "cherry_startvalue","cherry_endvalue","cherry_miylagtime","cherry_doubletime",
    "sapphire_testname","sapphire_hadError","sapphire_startvalue",
    "sapphire_endvalue","sapphire_miylagtime","sapphire_doubletime",
    "venus_testname","venus_hadError","venus_startvalue","venus_endvalue",
    "venus_miylagtime","venus_doubletime")
new_data <- all_data                                               #
# gets data file generated in Eve that contains negative and positive controls
#   and compounds (the program below can now cope with two lines per well
#   for each pos control & compound, where Eve generates such an output)
#
# (could replace this with algorithm to generate a csv-type matrix with 25
#  columns & one row for each negative control, positive control, and compound.)  
#
m <- dim(all_data)[1]
# gives the depth of the full data set to m
for (n in 0:m){
  if(!identical(all_data$id_chemical[n],"1313"))
    {all_data$id_chemical[n]<-"NA"}
  }
# this routine replaces all id_chemical not equal to 1313 in all_data with NA;
#   it takes ~7 minutes to run in 16700 rows (17 Feb version)
# For some unknown reason, it refuses to identify the first line of the dataset
#   if the counter starts at 1; starting the counter at zero fixes this
#   and is the only kludge I've used to date (comment typed 16 Feb 2011)
#
negctrl_split <- split(all_data, all_data$id_chemical)                         #
# sorts all_data by id_chemical again, so we should now have two groups
#   of data labelled as "1313" & "NA"                                          
#
negctrl_data <- negctrl_split$"1313"                                           #
# puts all lines where id_chemical is 1313 into negctrl_data
#
y <- as.table(summary(negctrl_data$id_mixture), row.names=TRUE)
# creates a summary counted list of the negctrl id_mixture labels
# if needed, a command for row labels could be:  z <- row.names(y)
#
new_y <- sort(y,decreasing=TRUE)
# sorts them into order of decreasing frequency so that top line is the mode,
#   and therefore the true identity of id_mixture for the negative control lines
# command for name of top frequency is: dimnames(new_y)[[1]][1]
# can compare against negctrl data set by:
#              dimnames(new_y)[[1]][1]==negctrl_data$id_mixture[1]
m <- dim(negctrl_data)[1]
# gives the length of the negctrl_data to m
for (n in 1:m){
  if (dimnames(new_y)[[1]][1]==negctrl_data$id_mixture[n]){
  negctrl_data$id_chemical[n]<-"1313"
  } else {
  negctrl_data$id_chemical[n]<-"NA"}
  }
# replaces id_chemical with NA if id_mixture isn't the same as the mode (i.e. a 
#       test for true negative control) (quick ~7 secs for 2470 lines, 17 Feb)
negctrl_split2 <- split(negctrl_data, negctrl_data$id_chemical)
negctrl_data2 <- negctrl_split2$"1313"  
# removes all lines that aren't true negative controls, and leaves the remainder
#   in negctrl_data for further processing                   #
neg_plate_means_list <- by(negctrl_data2,negctrl_data2$id_plate_instance,mean) #
neg_plate_stdev_list <- by(negctrl_data2,negctrl_data2$id_plate_instance,sd)
neg_plate_n_list <- by(negctrl_data2,negctrl_data2$id_plate_instance,dim)
# calculates and lists the means & sd of all negctrl attributes                #
neg_plate_means_df <- as.data.frame.list(neg_plate_means_list)                 #
neg_plate_stdev_df <- as.data.frame.list(neg_plate_stdev_list)
# changes list to data frame, giving a 25 rows, 45 columns of neg ctrl means/sd#
neg_plate_means <- data.frame(t(neg_plate_means_df))                           #
neg_plate_stdev <- data.frame(t(neg_plate_stdev_df))
# transposes the data frame, which is now a 45 row, 25 column beastie          #
neg_plate_means_matrix <- as.matrix.data.frame(neg_plate_means)
neg_plate_stdev_matrix <- as.matrix.data.frame(neg_plate_stdev)
# converts the neg ctrl data to a matrix form
#all_data_negctrlNA <- read.csv(filename, na.strings="1313")                    #
# reads filename & places NA wherever 1313 pops up (ID of DMSO)                #
#compound_data <- all_data_negctrlNA[!is.na(all_data_negctrlNA$id_chemical),]   #

m <- dim(neg_plate_means_matrix)[1]
plate <- array(1:m)
wells <- array(1:m)
cDT_mean <- array(1:m)
sDT_mean <- array(1:m)
vDT_mean <- array(1:m)
cDT_stdev <- array(1:m)
sDT_stdev <- array(1:m)
vDT_stdev <- array(1:m)

for (i in 1:m){
  plate[i] <- neg_plate_means_matrix[i,1]
  wells[i] <- neg_plate_n_list[[i]][1]
  cDT_mean[i] <- neg_plate_means_matrix[i,13]
  sDT_mean[i] <- neg_plate_means_matrix[i,19]
  vDT_mean[i] <- neg_plate_means_matrix[i,25]
  cDT_stdev[i] <- neg_plate_stdev_matrix[i,13]
  sDT_stdev[i] <- neg_plate_stdev_matrix[i,19]
  vDT_stdev[i] <- neg_plate_stdev_matrix[i,25]
  }  

negctrl_stats <- data.frame(plate,wells,cDT_mean,sDT_mean,vDT_mean,cDT_stdev,
               sDT_stdev,vDT_stdev)

write.csv(negctrl_stats,file="negctrl_stats.csv")          

# above code makes and reports a table of the negctrl stats for doubling time
# to be used later as a guide on whether the results are too noisy to be of use

m <- dim(new_data)[1]
# gives the length of the all_data to m
for (n in 1:m){
  if (new_data$id_chemical[n]==1313){
  new_data$id_chemical[n] <- NA
  }
  }
compound_data <- new_data[!is.na(new_data$id_chemical),]

##all_data_negctrlNA <- 0
# sorts out compound data by removing all lines where id_chemical is NA        #
# & resets all_data_negctrl to zero to remove weight on memory
##compound_data_means_list<-by(compound_data,compound_data$id_chemical,mean)   #
# calculates and lists the mean of all individual compound attributes          #
##compound_data_means_df<-as.data.frame.list(compound_data_means_list)         #
# changes list to data frame, giving a 25 rows, xx columns of means            #
##compound_means<-data.frame(t(compound_data_means_df))                        #
# transposes the data frame, which is now a 25 column beastie                  #
##compound_data_matrix <- as.matrix.data.frame(compound_means)
# does the same to the compound data
#
plates <- length(neg_plate_means_matrix[,1])
# the length of the plate_instance_id column (#1)
compounds <- length(compound_data[,1])
# the same thing, but for the compounds list
p <- 1
# p is a counter
# new_compound <- ("")
# list_plates <- ("")
#
compid <- array(1:compounds)
index <- array(1:compounds)
c_fluor <- array(1:compounds)
c_ratio <- array(1:compounds)
c_miylagtime <- array(1:compounds)
c_doubletime <- array(1:compounds)
s_fluor <- array(1:compounds)
s_ratio <- array(1:compounds)
s_miylagtime <- array(1:compounds)
s_doubletime <- array(1:compounds)
s_activity <- array(1:compounds)
v_fluor <- array(1:compounds)
v_ratio <- array(1:compounds)
v_miylagtime <- array(1:compounds)
v_doubletime <- array(1:compounds)
v_activity <- array(1:compounds)
# sets up various arrays for the calculations to be fed into
#
for (i in 1:compounds){
for (j in 1:plates){
#      if (identical(neg_plate_means_matrix[j,1],compound_data[i,1]))
       if (neg_plate_means_matrix[j,1]-compound_data[i,1]<1)
# makes sure the plate id for the negctrl data is the same as the compound
#   and then runs the follow set of calculations...
      {compid[i] <- compound_data[i,5]
      index[i] <- p
      c_fluor[i] <- (compound_data[i,10] - neg_plate_means_matrix[j,10])/
              (neg_plate_means_matrix[j,11] - neg_plate_means_matrix[j,10])
      c_ratio[i] <- (compound_data[i,11] - compound_data[i,10])/
              (neg_plate_means_matrix[j,11] - neg_plate_means_matrix[j,10])
      c_miylagtime[i] <- compound_data[i,12] - neg_plate_means_matrix[j,12]
      c_doubletime[i] <- compound_data[i,13]/neg_plate_means_matrix[j,13]
      s_fluor[i] <- (compound_data[i,16] - neg_plate_means_matrix[j,16])/
              (neg_plate_means_matrix[j,17] - neg_plate_means_matrix[j,16])
      s_ratio[i] <- (compound_data[i,17] - compound_data[i,16])/
              (neg_plate_means_matrix[j,17] - neg_plate_means_matrix[j,16])
      s_miylagtime[i] <- compound_data[i,18] - neg_plate_means_matrix[j,18]
      s_doubletime[i] <- compound_data[i,19]/neg_plate_means_matrix[j,19]
      s_activity[i] <- c_ratio[i] - s_ratio[i]
      v_fluor[i] <- (compound_data[i,22] - neg_plate_means_matrix[j,22])/
              (neg_plate_means_matrix[j,23] - neg_plate_means_matrix[j,22])
      v_ratio[i] <- (compound_data[i,23] - compound_data[i,22])/
              (neg_plate_means_matrix[j,23] - neg_plate_means_matrix[j,22])
      v_miylagtime[i] <- compound_data[i,24] - neg_plate_means_matrix[j,24]
      v_doubletime[i] <- compound_data[i,25]/neg_plate_means_matrix[j,25]
      v_activity[i] <- c_ratio[i] - v_ratio[i]
      j <- plates
#      results[i] <- c(compound_data_matrix[i,5], 
 #     c_fluor[i], c_ratio[i], c_miylagtime[i], c_doubletime[i],
  #    s_fluor[i], s_ratio[i], s_miylagtime[i], s_doubletime[i],
   #   v_fluor[i], v_ratio[i], v_miylagtime[i], v_doubletime[i])
      }
      }
      p <- p+1
      }
# this routine took ~7 minutes to sort through 6500 compounds, so would be
#   ~15 minutes for the full 14400 compounds of the MaybridgeHF library (17 Feb).
#
#a <- array(c_fluor,c_ratio,c(compounds,12))
all_ratios <- data.frame (compound_data$id_plate_instance,compound_data$well_row,
        compound_data$well_col,compid,compound_data$id_chemical,
        compound_data$name,compound_data$smiles,c_fluor,c_ratio,c_miylagtime,
        c_doubletime,s_fluor,s_ratio,s_miylagtime,s_doubletime,v_fluor,v_ratio,
        v_miylagtime,v_doubletime)
#results <- as.table(c_fluor,s_fluor)
#      
#  For Kurt's code, need to generate seven files for use in measurements:
#    1. Three column text file with 1st column having a line number of 1:n
#       2nd column having compid & 3rd column having SMILES code
#    2. Three column text file with 1st column being indexed 1:n
#       and 2nd column having compid & 3rd having c_ratio
#    3. Three column text file with 1st column being indexed 1:n
#       and 2nd column having compid & 3rd having s_ratio
#    4. Three column text file with 1st column being indexed 1:n
#       and 2nd column having compid & 3rd having v_ratio
#    5. Two column text file with 1st column being indexed 1:n
#       and 2nd column having c_ratio
#    6. Two column text file with 1st column being indexed 1:n
#       and 2nd column having s_ratio
#    7. Two column text file with 1st column being indexed 1:n
#       and 2nd column having v_ratio
#

indexed_id <- data.frame(index,compid,compound_data$smiles)
indexed_s_activity <- data.frame(index,s_activity)
indexed_v_activity <- data.frame(index,v_activity)
indexed_id_c_ratio <- data.frame(index,compid,c_ratio)
indexed_id_s_ratio <- data.frame(index,compid,s_ratio)
indexed_id_v_ratio <- data.frame(index,compid,v_ratio)
indexed_c_ratio <- data.frame(index,c_ratio)
indexed_s_ratio <- data.frame(index,s_ratio)
indexed_v_ratio <- data.frame(index,v_ratio)
unindexed_c_ratio <- as.vector(c_ratio)
# write(unindexed_c_ratio,"x.mat",ncolumns=1)
# this creates a 1 column list of the c_ratio as a .mat file for Kurt's code
# but don't really want this for ratios!! need it for smiles codes though
#
# NOTE: some ratios are reported as NA. Not sure yet if Kurt's code can handle
#   such items, so might need to be removed/reset

##write.table(data.matrix(indexed_id_c_ratio),"y.csv",sep=",",col.names=FALSE,row.names=FALSE)
##writes a csv-type table with no column or row names!!!
##might be able to concatenate process by choosing data.matrix straightaway

write.csv(indexed_id,file="screen_index.csv")
# 4plates_index.csv looks ok (21 Feb)
write.table(data.matrix(indexed_s_activity),"measurements_sapphire.mat",sep=",",
                col.names=FALSE,row.names=FALSE)  
write.table(data.matrix(indexed_v_activity),"measurements_venus.mat",sep=",",
                col.names=FALSE,row.names=FALSE)
write.csv(indexed_s_activity,file="original_measurements_sapphire.csv")
write.csv(indexed_v_activity,file="original_measurements_venus.csv")
# measurements.mat changed to sapphire & venus versions - 7 April
write.csv(all_ratios,file="activity_results_output.csv")                
# added 10 Mar                




################################################################################
#
# Step (iii): Split plate data into controls and compounds
#
# Open single plate data file
# Identify negative controls, and move them to a new file
# Remove and discard all other lines that are identified as DMSO
# Identify positive controls by identity in E (currently five compounds), and
#   remove them to a new file
# Save the remaining lines; this file should now just contain the data for
#   compounds on test
#
################################################################################

#filename <- "TS6_test.csv"
#all_data <- read.csv(filename)
#all_data_negctrlNA <- read.csv(filename, na.strings="1313")
#compound_data <- all_data_negctrlNA[!is.na(all_data_negctrlNA$id_chem),]
#negctrl_split <- split(all_data, all_data$id_chem)
#negctrl_data <- negctrl_split$"1313"

# so we now have two data sets: compound_data & negctrl_data (woop-woop)

################################################################################
#
# Step (iv): Calculate mean & standard deviation of negative controls
#
#                                                              
################################################################################

#negctrl_mean <- mean(negctrl_data)
#negctrl_sd <- sd(negctrl_data)


# Note: these will give NA for non-logical/numeric data

################################################################################
#
# Step (v): Check hadError column
#
# Should probably do this as part of step (i)
# Remove any lines where I, O or U <> 0
# Maybe perform this alongside removal of empty wells (but save them to a
#   different file
# Need to flag these up (and empty wells) in an error report for the batch run;
#   this report could also include information on negative control statistics
#
################################################################################


################################################################################
#
# Step (vi): Calculate values for each chromophore
#
# Load plate compound data
# Load plate negative control mean & variance data
# Perform calculations as listed above (The algorithm)
# Record & save results versus D, E, F & G (these results will form the nucleus
#   of subsequent analyses and comparisons)
#
################################################################################

#c_fluor <- (compound_data[10] - negctrl_mean[10])/
#              (negctrl_mean[11] - negctrl_mean[10])
#c_ratio <- (compound_data[11] - compound_data[10])/
#              (negctrl_mean[11] - negctrl_mean[10])
#c_miylagtime <- compound_data[12] - negctrl_mean[12]
#c_doubletime <- compound_data[13]/negctrl_mean[13]
#s_fluor <- (compound_data[16] - negctrl_mean[16])/
#              (negctrl_mean[17] - negctrl_mean[16])
#s_ratio <- (compound_data[17] - compound_data[16])/
#              (negctrl_mean[17] - negctrl_mean[16])
#s_miylagtime <- compound_data[18] - negctrl_mean[18]
#s_doubletime <- compound_data[19]/negctrl_mean[19]
#v_fluor <- (compound_data[22] - negctrl_mean[22])/
#              (negctrl_mean[23] - negctrl_mean[22])
#v_ratio <- (compound_data[23] - compound_data[22])/
#              (negctrl_mean[23] - negctrl_mean[22])
#v_miylagtime <- compound_data[24] - negctrl_mean[24]
#v_doubletime <- compound_data[25]/negctrl_mean[25]

#results <- as.data.frame(c(compound_data[4], 
#    c_fluor, c_ratio, c_miylagtime, c_doubletime,
#    s_fluor, s_ratio, s_miylagtime, s_doubletime,
#    v_fluor, v_ratio, v_miylagtime, v_doubletime))
    
#write.csv(results, file="dummy1.csv")

