#function [filledcurve,allsteps,percentmissing] = gapfill(curve,steps);
#%function [filledcurve,allsteps,percentmissing] = gapfill(curve,steps)
#%detects gaps in curve that are longer than mean of the sampling intervals
#%fills them using linear interpolation at a sampling frequency equal to the mean sampling frequency
#%returns the filled curve and the new timesteps

gapfill <- function(curve,steps){

npoints <- length(steps)
intervals <- steps[1:npoints-1]-steps[2:npoints]
meangap <- abs(mean(intervals)); #intervals are the intervals between samples
gaplocs <- which(abs(intervals)>2*meangap, arr.ind=T)
nongaplocs <- which(abs(intervals)<meangap, arr.ind=T) 
nongapmean <- mean(abs(intervals[nongaplocs])) #This is the mean of the intervals excluding the 'big' gaps
datam <- cbind(matrix(steps),matrix(curve))  ###PROBLEM WAS THAT THIS WAS A LIST BUT THE CODE ASSUMES MATRIX - ADDED coercion jjr 2/12/09
if (length(gaplocs)>0){
   #Now do a linear interpolation in the gaps found, at a sampling
   #frequency corresponding to nongapmean
   for (i in 1:length(gaplocs)){
    x <- c(steps[gaplocs[i]],steps[gaplocs[i]+1])
    y <- c(curve[gaplocs[i]],curve[gaplocs[i]+1])
    xi <- seq(steps[gaplocs[i]]+nongapmean,steps[gaplocs[i]+1]-nongapmean,by=nongapmean) #exclude existing pointsx
    yi <- linterp(x,y,xi)  ## added to replace previous line jjr 2/12/09 (not causing the problem below - making yi <- xi doesn't cure it)
    #Now append the interpolated steps to the original data
    extras <- cbind(xi,yi)
    datam <- rbind(datam,extras) 
   }
   #now sort by timesteps
   sorted <- sort(datam[,1],decreasing=F, index.return=T) 
   datam <- datam[sorted$ix,]
   filledcurve <- datam[,2]
   allsteps <- datam[,1]
}
else{
   filledcurve <- curve
   allsteps <- steps
}
percentmissing <- ceiling(100*(length(filledcurve)-length(curve))/length(filledcurve)) ##changed to ceiling jjr 2/12/09

list(filledcurve=filledcurve,allsteps=allsteps,percentmissing=percentmissing)				 
}
