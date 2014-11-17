# function [smoothcurve,smoothsteps,splvec] = slidespline4(noisecurve,tsteps,PointsPerRegion);
# function [smoothcurve,smoothsteps,splvec] = slidespline4(noisecurve,tsteps,PointsPerRegion);
# Spline smoothing with sliding windows.
# Divides curve into equal regions, takes first point from each and computes a spline curve
# then second point from each and so on. Finally takes the median of all the spline curves.
# noisecurve are the amplitude samples
# tsteps are the corresponding time points
# PointsPerRegion is the number of points in each region (bigger value obviously gives more smoothing)
# smoothcurve is the smoothed curve and smoothsteps the corresponding time points.
# splvec is the vector of samples and timepoints input to the spline algorithm (returned for diagnostics only)

slidespline <- function(noisecurve,tsteps,PointsPerRegion){

numpoints <- length(noisecurve)
offset <- 0
allcurves <- NULL							   
							   
pinpoints <- 3  #was 5 then 3.  changed jjr 17/2/10
j <- 1+offset

# compute sensible values to pin the splines at the start and finish points of the curve
startpin <- c(min(tsteps),median(noisecurve[1:pinpoints])) # starting point is among the first steps
endpin <- c(max(tsteps),median(noisecurve[(length(tsteps)-pinpoints):length(tsteps)])) # look among the last five timesteps

while (j <= PointsPerRegion){ #spline fitting in each window
  splvec<-NULL
  allpoints <- NULL
  #Build a spline vector
  while (j <= numpoints){
     allpoints <- c(allpoints,j)
     #print(allpoints)			
     lastpoint <- j
     j <- j+PointsPerRegion
  }
  splvec <- cbind(tsteps[allpoints],noisecurve[allpoints])
  
  # Add start and end points to splvec to pin the curve at the medians of data points 
  # at the start and finish of the curve
  if (max(splvec[,1]) != max(tsteps)){
     splvec <- rbind(splvec,endpin, deparse.level=0)
  }
  if (min(splvec[,1]) != min(tsteps)){
     splvec <- rbind(splvec,startpin, deparse.level=0)
  }

  ## then do a spline fit; extra zeros on spline vector force zero slope at extremities
  splcurve <- spline(splvec[,1],splvec[,2], n=4, method = "fmm",xmin = min(tsteps), xmax = max(tsteps), tsteps,) #replaces line above jjr 8/12/09 (xout specified so n not used)
  allcurves <- rbind(allcurves,splcurve$y, deparse.level=0)
  offset <- offset+1
  j <- offset
}# end 'while'

medvector <- NULL
for (i in 1:dim(allcurves)[2]){
   medvector <- c(medvector,median(allcurves[,i]))
}

smoothcurve <- medvector
smoothsteps <- tsteps

list(smoothcurve=smoothcurve,smoothsteps=smoothsteps,splvec=splvec,allcurves=allcurves)
}
	   
