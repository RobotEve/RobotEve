#%This function takes a vectors of OD or fluorescence values and corresponding timepoints and calculates the various returned values.

growthparams <- function(logcurve,timesteps){
numpoints <- length(logcurve)

threshmargin <- 15  # WAS 10 (jjr 11/3/10) #%percentage P of the maximum slope S; 
                   # portion of curve between first +ve crossing and last -ve crossing of S-(S*P/100) is considered linear

#%TAKE THE FIRST DERIVATIVE ON THE BASIS OF SINGLE INTERVALS ONLY
slopes <- logcurve[2:numpoints]-logcurve[1:(numpoints-1)];
timeintervals <- timesteps[2:numpoints]-timesteps[1:(numpoints-1)];  ### jjr 11/3/10
slopes <- slopes/timeintervals
slopes[numpoints] = (logcurve[numpoints] - logcurve[numpoints-1])/(timesteps[numpoints]-timesteps[numpoints-1]); #add the end point

#%TAKE 2nd DERIVATIVE
secderiv = slopes[2:numpoints] - slopes[1:(numpoints-1)]
secderiv <- secderiv/timeintervals
posindices = which(secderiv > 0, arr.ind=T)
negindices = which(secderiv <= 0, arr.ind=T)

if (length(posindices) < 1) {startvalidregion <- 1} #catch decay cases
else{startvalidregion= min(posindices)}

# The following added jjr 22/5/07 to prevent discard of *significant* initial positive slopes
if (mean(slopes[1:3]) > mean(slopes[4:length(slopes)])+sd(slopes[4:length(slopes)]))
{
 startvalidregion = 1 #%set it back to the first point in the growth curve
}

if (length(negindices) < 1) {endvalidregion <- length(secderiv)}
else{endvalidregion= max(negindices)}

# add 4/4/07 condition for lagtime so it's not Inf
overzero <- which(slopes > (0.00001), arr.ind=T) 
if (length(overzero)==0) lagtime <- 0 else lagtime = min(timesteps[slopes>(0.01*max(slopes))]) #jjr 11/3/10
maxslope = max(slopes[startvalidregion:endvalidregion]) #%taken on the log curve
maxgrowthtime = min(timesteps[slopes[startvalidregion:endvalidregion]==maxslope]) #%timepoint of peak growth rate 

#%First +ve crossing and last -ve crossing of the threshold 
#%are used to identify the linear region on the grounds that the region
#%between two points of maximum growth should represent max growth
#This calculation gives wrong thresh when maxslope is -ve but in that case
#it is definitely a decay curve and so will be flagged, not analysed, so this is not a problem

sortedslopes <- sort(slopes,decreasing = TRUE)
meanpeakslopes <- mean(sortedslopes[1:4]) #take max slope as the mean of the highest values
thresh <- meanpeakslopes - threshmargin*meanpeakslopes/100
                                     
#% Find linear region, linear growth rate, linear growth duration, doubling time
linearindices = which(slopes[startvalidregion:endvalidregion]>=thresh)
linearindices = linearindices+startvalidregion-1 #%restore values after removing the invalid start region. 

if (mean(slopes) > 0)  ##if slopes are predominantly +ve it is a 'normal' curve 
 {
  #Select the start points of the max growth region as the point closest to the first +ve crossing of the threshold
  if (min(linearindices != 1))   # ==1 is the difficult case - not catered for yet...
   {
   if (abs(slopes[min(linearindices)]-thresh) > abs(thresh - slopes[min(linearindices)-1]))
     {linearindices = c(min(linearindices)-1,linearindices)}
   }
  # End point is the one after the last negative crossing of the threshold
 if (max(linearindices) !=  length(slopes)) #%catch pathological case where it is already the last!
   {
   # A point on 'slopes' represents the 
   # slope between two data points so taking the next point on the growth curve is the correct thing to do  jjr 11/6/10
     linearindices = c(linearindices,max(linearindices)+1)
   }
  linearpart = timesteps[linearindices]
  startlinOD <- logcurve[min(linearindices)] 
  endlinOD <- logcurve[max(linearindices)]
  startlinear <- min(linearpart)
  endlinear <- max(linearpart)
 
  if (endlinear==startlinear) linearslope=0 else linearslope <- (endlinOD-startlinOD)/(endlinear-startlinear) 
  doubletime = 1/(linearslope +.00000000001); #remember that we are in log 2 space
 
  if (linearslope==0) miylagtime = startlinear - (startlinOD-min(logcurve))/(linearslope+0.00001) 
  else miylagtime = startlinear - (startlinOD-  min(logcurve))/linearslope 
  #%extrapolates the max growth back and takes lagtime as intersection with min OD value
  }
 else	# it is a decay curve 
  	 {
 	  startlinear <- 0
 	  endlinear <- 0
 	  startlinOD <- 0
 	  endlinOD <- 0
 	  linearslope <- 0
 	  doubletime <- 9999999 #effectively a numeric 'Inf' - because "growth" is negative! 
 	  miylagtime <- 0
 	  } 
 
newMaxValue <- 2^max(logcurve)
newMaxValueTime <- timesteps[min(which(logcurve >= max(logcurve)))] # time of first occurrence of the maximum value

newMinValue <- 2^min(logcurve)
newMinValueTime <- timesteps[min(which(logcurve <= min(logcurve)))] #time of first occurrence of the minimum value

StartValue <- 2^logcurve[1]
StartValueTime <- timesteps[1]
EndValue <- 2^logcurve[length(timesteps)]
EndValueTime <- timesteps[length(timesteps)]


list(StartValue=StartValue,StartValueTime=StartValueTime,EndValue=EndValue,EndValueTime=EndValueTime, 
MaxValueLin=newMaxValue,MaxValueTime=newMaxValueTime,MinValueLin=newMinValue,MinValueTime=newMinValueTime,
startlinear=startlinear,endlinear=endlinear,linearslope=linearslope,doubletime=doubletime,lagtime=lagtime,
miylagtime=miylagtime,log2curve=logcurve,timepointsout=timesteps,thresh=thresh,slopes=slopes)  
}
