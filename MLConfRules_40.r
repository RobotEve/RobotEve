################################################################################
#
# Program written in R
# Kevin Williams
# Test version, 21 Oct 2011
#
################################################################################
#
# Applying ML rules to EveActivity output for confirmation screen
#
################################################################################
#
# Input: a single .csv file of 18 columns for 3 strains; this will contain
#      well/plate location, compound info and parameters for start fluorescence,
#      growth, lagtime & doubling time.
#
# Main outputs: 
#
# Other outputs: 
#
# Column names:
#   A/1  no-title       : numeric, index no.
#   B/2  wellplate      : numeric, plate number
#   C/3  wellrow        : letter, always A-P
#   D/4  wellcolumn     : numeric, always 1-24
#   E/5  compid         : numeric, Eve ID for chemicals in well
#     compname       : string, full compound name
#   F/6  compsmiles     : string
#   G/7  c_fluor        : numeric, initial fluorescence
#   H/8  c_ratio        : numeric
#   I/9  c_miylagtime   : numeric
#   J/10  c_doubletime  : numeric
#   K/11  s_fluor       : numeric, initial fluorescence
#   L/12  s_ratio       : numeric
#   M/13  s_miylagtime  : numeric
#   N/14  s_doubletime  : numeric
#   O/15  v_fluor       : numeric, initial fluorescence
#   P/16  v_ratio       : numeric
#   Q/17  v_miylagtime  : numeric
#   R/18  v_doubletime  : numeric
#
################################################################################
#
# The algorithm (as used in the ML screen work earlier)
#
#  (i) Identify self-fluorescing compounds
#
#         Fluoro if G, K or O >0.08
#
# (ii) Identify potential hits (score = 2)
#
#         DefActive if H, L or P <0.8
#
#(iii) Identify lower activity (score = 1 per activity)
#
#         PossActive if not DefActive, and (I/M/Q >4 and/or J/N/R >1.5)
#
# (iv) DefToxic if: all DefActive or doubly PossActive (i.e. total score = 6)
#
#  (v) ProbToxic if: either DefActive or mostly PossActive (total score = 5)
#
# So, remove fluoro, then remove DefToxic, then remove ProbToxic.
# Can then label remaining as c, s & v hits, or as inactive.
#
#
# Then... also produce 
#
#
#
#
################################################################################
#
# 
#

rm(list=ls(all=TRUE))
# removes all previous variables, preventing cross-contamination

start_time <- format(Sys.time(),"%Y%m%d%H%M%S")

indexed_compounds <- read.csv("indexed_compounds.csv")
#indexed_compounds$X <- NULL
colnames(indexed_compounds)[1] <- "compid"
colnames(indexed_compounds)[2] <- "cmpd_conc_um"
colnames(indexed_compounds)[3] <- "cmpd_index"

filename <- "activity_results_output.csv"
activity_data <- read.csv(filename)
activity_data$X <- NULL 
colnames(activity_data)[1] <- "plate"
colnames(activity_data)[2] <- "well_row"
colnames(activity_data)[3] <- "well_col"
colnames(activity_data)[5] <- "id_chemical"
colnames(activity_data)[6] <- "name"
colnames(activity_data)[7] <- "smiles"
c_ratio <- activity_data$c_ratio
m <- dim(activity_data)[1]
# gives the depth of the full data set to m
a <- 0
c_fluor_tag <- array(1:m)
c_score <- array(1:m)
s_fluor_tag <- array(1:m)
s_score <- array(1:m)
v_fluor_tag <- array(1:m)
v_score <- array(1:m)
total_score <- array(1:m)
fluor_score <- array(1:m)
ratio_total <- array(1:m)
c_tot_ratio <- array(1:m)
s_tot_ratio <- array(1:m)
v_tot_ratio <- array(1:m)
c_s_ratio <- array(1:m)
c_v_ratio <- array(1:m)
s_c_ratio <- array(1:m)
s_v_ratio <- array(1:m)
v_c_ratio <- array(1:m)
v_s_ratio <- array(1:m)


for (n in 1:m){
  c_fluor_tag[n] <- 0
  c_score[n] <- 0
  s_fluor_tag[n] <- 0
  s_score[n] <- 0
  v_fluor_tag[n] <- 0
  v_score[n] <- 0
  
  ratio_total[n] <- activity_data$c_ratio[n] + activity_data$s_ratio[n] + 
                    activity_data$v_ratio[n]
  c_tot_ratio[n] <- activity_data$c_ratio[n]/ratio_total[n]
  s_tot_ratio[n] <- activity_data$s_ratio[n]/ratio_total[n]
  v_tot_ratio[n] <- activity_data$v_ratio[n]/ratio_total[n]
  c_s_ratio[n] <-  activity_data$c_ratio[n]/activity_data$s_ratio[n]
  c_v_ratio[n] <-  activity_data$c_ratio[n]/activity_data$v_ratio[n]
  s_c_ratio[n] <-  activity_data$s_ratio[n]/activity_data$c_ratio[n]
  s_v_ratio[n] <-  activity_data$s_ratio[n]/activity_data$v_ratio[n]
  v_c_ratio[n] <-  activity_data$v_ratio[n]/activity_data$c_ratio[n]
  v_s_ratio[n] <-  activity_data$v_ratio[n]/activity_data$s_ratio[n]
  
  
  if (activity_data$c_fluor[n] > 0.08)
    {c_fluor_tag[n] <- 1}
  if (activity_data$s_fluor[n] > 0.08)
    {s_fluor_tag[n] <- 1
    a <- a+1}
  if (activity_data$v_fluor[n] > 0.08)
    {v_fluor_tag[n] <- 1}
# above if statements check whether compound is fluoro for any of the filters
  fluor_score[n] <-  c_fluor_tag[n] + s_fluor_tag[n] + v_fluor_tag[n]
  if (fluor_score[n] < 1)
# if compound isn't fluoro... give it an activity score (2=hit, 1=PossActive):
    {if (activity_data$c_ratio[n] < 0.8)
        {c_score[n] <- 2}
        else
        {if (activity_data$c_miylagtime[n] > 4)
            {c_score[n] <- c_score[n] + 1}
        if (activity_data$c_doubletime[n] > 1.5)
            {c_score[n] <- c_score[n] + 1}
            }
    if (activity_data$s_ratio[n] < 0.8)
        {s_score[n] <- 2}
        else
        {if (activity_data$s_miylagtime[n] > 4)
            {s_score[n] <- s_score[n] + 1}
        if (activity_data$s_doubletime[n] > 1.5)
            {s_score[n] <- s_score[n] + 1}
            }
    if (activity_data$v_ratio[n] < 0.8)
        {v_score[n] <- 2}
        else
        {if (activity_data$v_miylagtime[n] > 4)
            {v_score[n] <- v_score[n] + 1}
        if (activity_data$v_doubletime[n] > 1.5)
            {v_score[n] <- v_score[n] + 1}
            }
        }
    total_score[n] <- c_score[n] + s_score[n] + v_score[n]
    }
# end of "for" loop    
    
fluor_score <- as.data.frame(fluor_score)
total_score <- as.data.frame(total_score)
c_score <- as.data.frame(c_score)
s_score <- as.data.frame(s_score)
v_score <- as.data.frame(v_score)

ratio_total <- as.data.frame(ratio_total)
c_tot_ratio <- as.data.frame(c_tot_ratio)
s_tot_ratio <- as.data.frame(s_tot_ratio)
v_tot_ratio <- as.data.frame(v_tot_ratio)

c_s_ratio <- as.data.frame(c_s_ratio)
c_v_ratio <- as.data.frame(c_v_ratio)
s_c_ratio <- as.data.frame(s_c_ratio)
s_v_ratio <- as.data.frame(s_v_ratio)
v_c_ratio <- as.data.frame(v_c_ratio)
v_s_ratio <- as.data.frame(v_s_ratio)

analysed_data <- data.frame(activity_data$plate,activity_data$well_row,
  activity_data$well_col,activity_data$compid,activity_data$id_chemical,
  activity_data$name,activity_data$smiles,activity_data$c_ratio,
  activity_data$s_ratio,activity_data$v_ratio,fluor_score,total_score,
  c_score,s_score,v_score,ratio_total,c_tot_ratio,s_tot_ratio,v_tot_ratio,
  c_s_ratio,c_v_ratio,s_c_ratio,s_v_ratio,v_c_ratio,v_s_ratio)

# sort data
analysed_data_fluoro <- analysed_data[order(-fluor_score),]
# gives data frame with high fluoro scores at top of list
analysed_data_toxic <- analysed_data[order(fluor_score,-total_score),]
# gives data frame with high fluoro at bottom of list, then sorts remainder
#   to give high total score (try and use DefTox=6,ProbTox=5) at top
analysed_data_c_hit <- analysed_data[order(fluor_score,-c_score,total_score),]
analysed_data_s_hit <- analysed_data[order(fluor_score,-s_score,total_score),]
analysed_data_v_hit <- analysed_data[order(fluor_score,-v_score,total_score),]
# these give data frames with fluoro at end of list, and sort the respective
#   hits with cleanest at the top, moving through higher total scores until
#   you get the ProbTox & DefTox ones, then all the inactive ones (with fluoro
#   last)

a <- 0
for (n in 1:m){
  if (analysed_data_fluoro[n,11] < 1)
    {n <- m}
    else
    {a <- a + 1}  
}
fluoro_compounds <- analysed_data_fluoro[1:a,]
write.csv(fluoro_compounds,file="fluoro_compounds.csv")
# generates a list of fluorescent compounds

a <- 0
b <- 0
for (n in 1:m){
  if (analysed_data_toxic[n,12] < 5)
    {n <- m}
    else
    {a <- a + 1}  
}
toxic_compounds <- analysed_data_toxic[1:a,]
write.csv(toxic_compounds,file="toxic_compounds.csv")

a <- 0
for (n in 1:m){
  if (analysed_data_c_hit[n,13] > 1)
    {if (analysed_data_c_hit[n,12] < 6)
      {a <- a + 1}  
}   }
c_hit_compounds <- analysed_data_c_hit[1:a,]
write.csv(c_hit_compounds,file="cherry_hits.csv")


a <- 0
for (n in 1:m){
  if (analysed_data_s_hit[n,14] > 1)
    {if (analysed_data_s_hit[n,12] < 6)
      {a <- a + 1}  
    }
}
s_hit_compounds <- analysed_data_s_hit[1:a,]
write.csv(s_hit_compounds,file="sapphire_hits.csv")


a <- 0
for (n in 1:m){
  if (analysed_data_v_hit[n,15] > 1)
    {if (analysed_data_v_hit[n,12] < 6)
      {a <- a + 1}  
    }
}
v_hit_compounds <- analysed_data_v_hit[1:a,]
write.csv(v_hit_compounds,file="venus_hits.csv")

# for confirmation screen ML results, make a list of cherry, sapphire & venus
#   activity versus the index list
#
# use cbind to attach columns from indexed_compounds to the indexed list
#
#
reindexed_analysed_data <- cbind(indexed_compounds,analysed_data)

compound_split <- split(reindexed_analysed_data, reindexed_analysed_data$compid)

m <- length(compound_split)
cmpd_id <- array(1:m)
sum_c_score <- array(1:m)
sum_s_score <- array(1:m)
sum_v_score <- array(1:m)
rank_c_score <- array(1:m)
rank_s_score <- array(1:m)
rank_v_score <- array(1:m)
toxicity <- array(1:m)
sap_activity <- array(1:m)
ven_activity <- array(1:m)

for (n in 1:m){
  cmpd_id[n] <- compound_split[[n]][1][1,]
  sum_c_score[n] <- sum(compound_split[[n]][16])
  sum_s_score[n] <- sum(compound_split[[n]][17])
  sum_v_score[n] <- sum(compound_split[[n]][18])
  if (sum_c_score[n] > 9){
  toxicity[n] <- "Possibly toxic"
  } else {
  toxicity[n] <- "No toxicity indicated"
  }
  if (sum_s_score[n] > 9){
  sap_activity[n] <- "Sapphire active"
  } else {
  sap_activity[n] <- ""
  }
  if (sum_v_score[n] > 9){
  ven_activity[n] <- "Venus active"
  } else {
  ven_activity[n] <- ""
  }
  }

confirm_summary <- cbind(cmpd_id, sum_c_score, sum_s_score, sum_v_score,
   sap_activity, ven_activity, toxicity)
confirm_summary_df <- as.data.frame(confirm_summary)

sapphire_sorted <- confirm_summary_df[order(toxicity,-sum_s_score),]
venus_sorted <- confirm_summary_df[order(toxicity,-sum_v_score),]

write.csv(confirm_summary_df,file="confirmation_activity_by_compound.csv")
write.csv(sapphire_sorted,file="sapphire_sorted.csv")
write.csv(venus_sorted,file="venus_sorted.csv")
