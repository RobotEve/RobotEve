OutlierDump <- function(Tsteps,rawcurve,smoothcurve,sdmultiple){

difference <- abs(smoothcurve-rawcurve)
diffmean <- mean(difference)
sdev <- sd(as.vector(difference))

#percentdiffmean <- 100*diffmean/(max(rawcurve)-min(rawcurve))
#print("percentdiffmean ")
#print(percentdiffmean)

outlierloc <- which(abs(difference)> sdmultiple*sdev, arr.ind=T)
keep <- which(abs(difference) <= sdmultiple*sdev, arr.ind=T)
dumped <- rawcurve[outlierloc]
dumpedtsteps <- Tsteps[outlierloc]
cleancurve <- rawcurve[keep]
cleanTsteps <- Tsteps[keep]

#dev.set(6)
#plot(Tsteps,rawcurve,col = "blue",main = "red are dumped outliers")
#points(Tsteps,smoothcurve,col = "green")
#if (length(dumped) != 0){points(dumpedtsteps,dumped,pch = 20, col = "red")}
#points(cleanTsteps,cleancurve,pch=20,col="black")

list(cleancurve=cleancurve,Tsteps=cleanTsteps)
}
