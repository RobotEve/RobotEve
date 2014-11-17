FirstSmooth <- function(curveValues,curveTimePoints,numregions) 
{
curveT <- curveTimePoints
curveOD <- curveValues

nontrunclength <- length(curveTimePoints)
PointsPerRegion <- (ceiling(nontrunclength/numregions)) # parameterised jjr 8/3/10

# form splines on a sliding set of points (successive points from each window) and then take median of those splines
slidespl <- slidespline(curveValues,curveTimePoints,PointsPerRegion)
smoothcurve <- slidespl$smoothcurve
smoothsteps <- slidespl$smoothsteps
splvec <- slidespl$splvec
allcurves <- slidespl$allcurves

#set any points <= zero to 'zero'; measurement value can never be negative!
smoothcurve[smoothcurve <= 0] <- 0.0001  #was < 0 changed to <= jjr 1/12/09

#Now a light smoothing using only the first point of each window, and the final point.
regspl <- regspline(smoothcurve,smoothsteps,ceiling(PointsPerRegion/3))  ###was ceil - changed jjr 1/12/09
smoothcurve <- regspl$osmoothcurve
smoothsteps <- regspl$osmoothsteps
splvec <- regspl$osplvec

#set any points <= zero to zero; measurement value can never be negative!
smoothcurve[smoothcurve <= 0] <-  0.0001   #was < 0 changed to <= jjr 1/12/09

FirstSmooth <- smoothcurve  #for plotting only
list(smoothcurve=smoothcurve,smoothsteps=smoothsteps)
}
