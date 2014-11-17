#function [smoothcurve,smoothsteps,splvec] = regspline4(noisecurve,tsteps,regionsize);
#%function [smoothcurve,smoothsteps,splvec] = regspline4(noisecurve,tsteps,regionsize);
#%uses splines to smooth a curve using the first points of a number of regions, plus the final point.
#%suitable for use only on monotonic curves with small discontinuities.
#%noisecurve are the amplitude samples
#%tsteps are the corresponding time points
#%regionsize is the number of points in each region (bigger value obviously gives more smoothing)
#%smoothcurve is the smoothed curve and smoothsteps the corresponding time points.
#%splvec is the vector of samples and timepoints input to the spline algorithm (returned for diagnostics only)

regspline <- function(noisecurve,tsteps,regionsize){
startpoint <- 0
allpoints <- NULL
numpoints <- length(noisecurve)
j <- 1

while (j <= numpoints){
   allpoints <- c(allpoints,j)
   lastpoint <- j
   j <- j+regionsize
}

#%Start point is already included. If end point is absent, add it.
finalpoint <- length(noisecurve)
if (lastpoint != numpoints) allpoints <- c(allpoints,finalpoint)
splvec <- cbind(tsteps[allpoints],noisecurve[allpoints])

# then do a spline fit 
# extra zeros on the spline vector force zero slopes at extremities
smoothcurve <- spline(splvec[,1],splvec[,2], n=4, method = "fmm",xmin = min(tsteps), xmax = max(tsteps), tsteps,) #replaces line above jjr 8/12/09 (xout specified so n not used)
smoothcurve <- smoothcurve$y

smoothsteps <- tsteps

list(osmoothcurve=smoothcurve,osmoothsteps=smoothsteps,osplvec=splvec)
}
