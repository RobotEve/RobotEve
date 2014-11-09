################################################################################
#
# Program written in R
# Kevin Williams
# 14 April 2011
#
# 
#
################################################################################
#
#  A .xml file for feeding back into Eve, containing the compound IDs
#       (as xml_list.xml and xml_list.txt)
#   
################################################################################

filename <- "list_for_cherrypick.csv"
compounds <- read.csv(filename,header=FALSE)
n <- dim(compounds)[1]

xml_list <- array(1:(n+2))
xml_list[1] <- "<compounds>"
xml_list[n+2] <- "</compounds>"

for (i in 1:n){
  xml_list[i+1] <- paste("<compound><id>",compounds[i,1],"</id></compound>",sep="")
  }
write.table(data.matrix(xml_list),"xml_list.xml",quote=FALSE,
                sep=",",col.names=FALSE,row.names=FALSE)
write.table(data.matrix(xml_list),"xml_list.txt",quote=FALSE,
                sep=",",col.names=FALSE,row.names=FALSE)
