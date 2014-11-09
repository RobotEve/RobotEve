################################################################################
#
# Program written in R
# Kevin Williams
# 18 April 2011
#
# 19 April: modified to save the plot as a .pdf file
# 20 April: modified to include a plot as a bitmap (.bmp), and relabelled
#           y-axis as "Normalised growth"
# 23 May: modified to include a plot as a png file (.png)
# 14 June: modified to have upper range of 10um for all runs after 5 June
#
################################################################################
#
# Code for generating graphs from cherrypick data
#   
################################################################################
#
# 1. get dataset
# 2. sort into negative controls and compound data (by id_chemical)
# 3. calculate mean of negative controls, for normalisation
# 4. normalise compound fluorescence data
# 5. plot normalised compound data versus compound concentration
# 6. save .pdf file of plot
#
################################################################################
#
# Input: a single .csv file of 29 columns for 3 strains; this will initially
#   contain data from 12 plates, each having 8 replicates of 8 compounds at 
#   6 concentrations (0, 1, 2.5, 5, 10 & 20 um).
# note: this changed to 4 replicates at 6 concentrations for confirmation
#   screens after 5 June 2011 (0, 0.5, 1, 2.5, 5 & 10 um).
#
# The program will need to use the zero concentration compounds as the 
#   negative controls.
#
# Main outputs: .pdf graphs of the cherrypick results
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
#   Z/26  id_plate layout    : numeric
#  AA/27  id_study           : numeric
#  AB/28  vol_cmpd_nl        : numeric (0, 2.5, 5, 12.5, 25, 50)
#  AC/29  cmpd_conc_um       : numeric (0, 0.5, 1, 2.5, 5, 10)
#
################################################################################

# get the input data from the .csv file, and adds the column headings

filename <- "compounds.csv"
all_data <- read.csv(filename,header=FALSE)

colnames(all_data) <- c("id_plate_instance","well_row","well_col","id_mixture",
    "id_chemical","name","smiles","cherry_testname","cherry_hadError",
    "cherry_startvalue","cherry_endvalue","cherry_miylagtime","cherry_doubletime",
    "sapphire_testname","sapphire_hadError","sapphire_startvalue",
    "sapphire_endvalue","sapphire_miylagtime","sapphire_doubletime",
    "venus_testname","venus_hadError","venus_startvalue","venus_endvalue",
    "venus_miylagtime","venus_doubletime","id_plate_layout","id_study",
    "vol_cmpd_nl","cmpd_conc_um")    


################################################################################
#### this next section taken from EveActivity_loop #############################
################################################################################
cherrysplit <- split(all_data, all_data$cmpd_conc_um)
cherry_neg_ctrl <- cherrysplit$"0"
cherry_neg_plate_means_list <- by(cherry_neg_ctrl,cherry_neg_ctrl$id_plate_instance,mean)
cherry_neg_plate_stdev_list <- by(cherry_neg_ctrl,cherry_neg_ctrl$id_plate_instance,sd)
cherry_neg_plate_n_list <- by(cherry_neg_ctrl,cherry_neg_ctrl$id_plate_instance,dim)
# calculates and lists the means & sd of all negctrl attributes
cherry_neg_plate_means_df <- as.data.frame.list(cherry_neg_plate_means_list)
cherry_neg_plate_stdev_df <- as.data.frame.list(cherry_neg_plate_stdev_list)
# changes list to data frame
cherry_neg_plate_means <- data.frame(t(cherry_neg_plate_means_df))                           #
cherry_neg_plate_stdev <- data.frame(t(cherry_neg_plate_stdev_df))
# transposes the data frame
cherry_neg_plate_means_matrix <- as.matrix.data.frame(cherry_neg_plate_means)
cherry_neg_plate_stdev_matrix <- as.matrix.data.frame(cherry_neg_plate_stdev)
# converts the neg ctrl data to a matrix form
# the data set (plate no.s & numerical) are the same as for the previous loop version
#
m <- dim(cherry_neg_plate_means_matrix)[1]
plate <- array(1:m)
wells <- array(1:m)
cDT_mean <- array(1:m)
sDT_mean <- array(1:m)
vDT_mean <- array(1:m)
cDT_stdev <- array(1:m)
sDT_stdev <- array(1:m)
vDT_stdev <- array(1:m)

for (i in 1:m){
  plate[i] <- cherry_neg_plate_means_matrix[i,1]
  wells[i] <- cherry_neg_plate_n_list[[i]][1]
  cDT_mean[i] <- cherry_neg_plate_means_matrix[i,13]
  sDT_mean[i] <- cherry_neg_plate_means_matrix[i,19]
  vDT_mean[i] <- cherry_neg_plate_means_matrix[i,25]
  cDT_stdev[i] <- cherry_neg_plate_stdev_matrix[i,13]
  sDT_stdev[i] <- cherry_neg_plate_stdev_matrix[i,19]
  vDT_stdev[i] <- cherry_neg_plate_stdev_matrix[i,25]
  }  

negctrl_stats <- data.frame(plate,wells,cDT_mean,sDT_mean,vDT_mean,cDT_stdev,
               sDT_stdev,vDT_stdev)

write.csv(negctrl_stats,file="negctrl_stats_loop.csv")

################################################################################
################################################################################

# create an array containing all the plate numbers in the cherrypick screen
 
p <- dim(cherry_neg_plate_means_matrix)[1]
plate_num <- array(1:p)
for (k in 1:p){
    plate_num[k] <- cherry_neg_plate_means_matrix[k,1]
    }

# split all the data by id_chemical for individual graphing

dataset_split <- split(all_data, all_data$id_chemical)
n <- length(dataset_split)

for (i in 1:n){
    b <- as.vector(dataset_split)[[i]]   # set b to contain all lines of data
                                         #   for chemical[i]
    a <- b[1,5]                  # set a to be the id number of chemical[i]
    aa <- b[1,1]                 # set aa to be the plate id of chemical[i]

# set up a loop to check if a negctrl plate is same as plate for chemical[i]
    for (k in 1:p){              
        if (plate_num[k]==aa){
            if (!(a==1313)){
        ############# graph drawing, id & saving routine  ###########
                m <- dim(b)[1]           # set m to lines of data in chemical[i]
                cherry <- array(1:m)
                sapphire <- array(1:m)
                venus <- array(1:m)
                conc_um <- array(1:m)
                for (j in 1:m){          # calculate ratios vs plate negctrl
                    cherry[j] <- b[j,11]/cherry_neg_plate_means_matrix[k,11]
                    sapphire[j] <- b[j,17]/cherry_neg_plate_means_matrix[k,17]
                    venus[j] <- b[j,23]/cherry_neg_plate_means_matrix[k,23]
                    conc_um[j] <- b[j,29]
                    }
                graph_xlim <- c(0,10)    # set x limits of 0 to 20
                graph_ylim <- c(0,3)   # set y limits of 0 to 3
                
                plot_name <- as.character(a)
                plot_name_pdf <- paste(plot_name,".pdf",sep="")  # build .pdf name
                plot_name_bmp <- paste(plot_name,".bmp",sep="")  # build .bmp name
                plot_name_png <- paste(plot_name,".png",sep="")  # build .png name
                
                pdf(plot_name_pdf)           # turns on the 'save as pdf' function
                plot(graph_xlim,graph_ylim,main=b[1,5],xlab="Concentration, um",
                     ylab="Normalised growth",col=NULL)
                points(conc_um, cherry, col="red",pch=19,cex=0.7)
                points(conc_um, sapphire, col="blue",pch=19,cex=0.7)
                points(conc_um, venus, col="green",pch=19,cex=0.7)    
                # note: pch=19 means a round spot, cex=0.7 means a spot size
                #     of 0.7x the default size
                dev.off()                # turns off the 'save as pdf' function
                
                bmp(plot_name_bmp)           # turns on the 'save as bmp' function
                plot(graph_xlim,graph_ylim,main=b[1,5],xlab="Concentration, um",
                     ylab="Normalised growth",col=NULL)
                points(conc_um, cherry, col="red",pch=19,cex=0.7)
                points(conc_um, sapphire, col="blue",pch=19,cex=0.7)
                points(conc_um, venus, col="green",pch=19,cex=0.7)    
                # note: pch=19 means a round spot, cex=0.7 means a spot size
                #     of 0.7x the default size
                dev.off()                # turns off the 'save as bmp' function
                
                png(plot_name_png)           # turns on the 'save as png' function
                plot(graph_xlim,graph_ylim,main=b[1,5],xlab="Concentration, um",
                     ylab="Normalised growth",col=NULL)
                points(conc_um, cherry, col="red",pch=19,cex=0.7)
                points(conc_um, sapphire, col="blue",pch=19,cex=0.7)
                points(conc_um, venus, col="green",pch=19,cex=0.7)    
                # note: pch=19 means a round spot, cex=0.7 means a spot size
                #     of 0.7x the default size
                dev.off()                # turns off the 'save as png' function
                
        ############# end of graph drawing routine #############################
            }
        }
    }
}       # go make a cup of tea before you eyeball all the new spangly graphs
