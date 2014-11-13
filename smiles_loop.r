################################################################################
#
# Program written in R
# Kevin Williams
# Test version, updated 21 February 2011
#
# 
#
################################################################################
#
# Identifying SMILES codes of experiments being run by Robot Eve; these codes
#      can then be fed into Kurt's process (via OpenBabel) so that the next set 
#      of compounds can be selected
#
#
################################################################################
#
# 21 Feb: setting up filenames to run a test in the active learning loop
# 09 Mar: routine takes FAR too long to run (20 hours on Cledwall-testing to
#      sort through a full MaybridgeHF screen.  New plan: join the two lists 
#      (tested & new compounds together) and weed out the replicates in situ.
#      So:
#         get lists
#         index new compounds to follow on from tested compounds index
#         join lists together
#         primary sort alphabetical on smiles, secondary sort on index ascending
#         get length of new list
#         do a recursion and remove repeats that have come from the new list 
#             only (can do this on index no.)
#         save new list
#         have an improbably nice fresh hot cup of tea
# 11 Mar: this version (originally filename 110309_smiles.r) is being used with
#     the loop of the cherrypick simulation and first run
#
################################################################################
#
# Input: Two files - one of tested compounds, one of library compounds to
#      be evaluated.
#
# Main outputs: 
#   1. A single column vector listed as a .smi file containing SMILES
#      codes for all tested compounds, plus those from the library list not
#      already included in the first section.
#   2. A three column vector/table listing the index number, compound id, and
#      SMILES codes; this file will be used later on outputs associated with
#      single column vectors to identify which indexed compounds have been run)
#   3. A single column vector containing the index numbers of compounds that
#      are available for testing (saved as AvailMols.mat) for feeding directly
#      in to Kurt's code 
#
################################################################################

index1 <- "combined_index.csv"
index2 <- "unknown_compounds.csv"
measured_compounds <- read.csv(index1)
new_compounds <- read.csv(index2)
a <- new_compounds
# gets the two lists of compounds
m <- dim(measured_compounds)[1]
n <- dim(new_compounds)[1]
for (k in 1:n){
  new_compounds$index[k] <- n+k
  }
# re-indexes the new_compounds so that they follow on from index of 
#   measured_compounds
measured_compounds$X <- NULL
all_compounds <- merge(measured_compounds, new_compounds, all=TRUE)
all_compounds_sorted <- all_compounds[order(all_compounds$compound_data.smiles,all_compounds$index),]
reduced_compounds <- all_compounds_sorted

p <- dim(all_compounds_sorted)[1]
count <- 1
nom <- -1
for (i in 2:p){
x <- all_compounds_sorted$index[i] > m
if (x){
  nom <- nom+1
  y <- identical(as.character(all_compounds_sorted$compound_data.smiles[i]),
        (as.character(all_compounds_sorted$compound_data.smiles[i-count])))
  if (y){
    reduced_compounds <- reduced_compounds[-(i-nom),]
    count <- count+1
    }
  count <- 1
  }
  }
reduced_compounds_sorted <- reduced_compounds[order(reduced_compounds$index),]

r <- dim(reduced_compounds_sorted)[1]
for (j in 1:r){
  reduced_compounds_sorted$index[j] <- j
  }
# re-indexes the list, keeping the tested compounds with the same index number
#   as previous

s <- m+1
# the index no. for the first new compound
t <- r-m
# the total no. of new compounds
new_compounds <- array(1:t)
# sets an array of length t
for (k in s:r){
  new_compounds[k-s+1] <- reduced_compounds_sorted$index[k] 
  }

write(new_compounds,"AvailMols_loop.mat",ncolumns=1)  
# this creates a 1 column list of the index of untested molecules as the
#    input file "AvailMols.mat" for Kurt's Octave code "MultiSelOpt.m"

write.csv(reduced_compounds_sorted,file="all_compounds_indexed_loop.csv") 
# this creates an indexed list of compound ids and their SMILES codes for use
# to reidentify the compounds after running Kurt's code

all_smiles <- as.vector(reduced_compounds_sorted$compound_data.smiles)
write(all_smiles,"AllSMILES_loop.smi",ncolumns=1)
# this creates a single column charater vector of SMILES codes for the combined
# list of tested and untested compounds; to be loaded into OpenBabel in order to
# build the fingerprint sequence for Kurt's code
