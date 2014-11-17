source("./gapfill.R") 
source("./growthparams.R")
source("./regspline.R")
source("./OutlierDump.R")
source("./slidespline.R")
source("./linterp.R") #added jjr 4/12/09
source("./zespline.R") #added jjr 4/12/09
source("./FirstSmooth.R")
    
ProcessEveGrowthCurve <- function(origcurve,origtsteps,title,plots){

#FIRST FILL ANY GAPS BY LINEAR INTERPOLATION  
filled <- gapfill(origcurve,origtsteps)  
filledcurve <- filled$filledcurve    
filledtsteps <- filled$allsteps					      
percentmissing <-filled$percentmissing
filledcurve[filledcurve <= 0] <- 0.0001  #added jjr 5/12/09 (falsely -ve values were causing a problem with diagnostic plot)

###### INITIAL SMOOTHING #############
numregions <- 20 #was 15 #was 10 jjr 11/5/10
FirstSmooth <- FirstSmooth(filledcurve,filledtsteps,numregions)
FirstSmoothCurve <- FirstSmooth$smoothcurve
FirstSmoothSteps <- FirstSmooth$smoothsteps

################ next four experimentally added jjr 14/5/10 #################
FirstSmoothRMSnoise  <- 0.0001 + sqrt(mean(FirstSmoothCurve-filledcurve)^2)
FirstSmoothsnratio <- (max(FirstSmoothCurve)-min(FirstSmoothCurve))/FirstSmoothRMSnoise

#Now dump outliers based on this smoothing
sdmultiple <- 2  ### was 1.5 jjr 11/5/10
CleanCurve <- OutlierDump(filledtsteps,filledcurve,FirstSmoothCurve,sdmultiple)
cleanTsteps <- CleanCurve$Tsteps
cleancurve <- CleanCurve$cleancurve
percentremoved<-100*(length(FirstSmoothSteps)-length(cleanTsteps))/length(FirstSmoothSteps)

numregions <-15 
SecondSmooth <- FirstSmooth(cleancurve,cleanTsteps,numregions)

smoothcurve <- SecondSmooth$smoothcurve
smoothsteps <- SecondSmooth$smoothsteps

###Clip the curve so that it is never negative 
smoothcurve[smoothcurve < 0] <- 0

################ next four experimentally added jjr 14/5/10 #################
SecondSmoothRMSnoise  <- 0.0001 + sqrt(mean(smoothcurve-cleancurve)^2)
SecondSmoothsnratio <- (max(smoothcurve)-min(smoothcurve))/SecondSmoothRMSnoise

### if the FirstSmoothsnratio is better than the SecondSmoothsnratio then perform all further processing on the FirstSmooth curve.
if (FirstSmoothsnratio > SecondSmoothsnratio) 
 {
  smoothcurve <- FirstSmoothCurve
  smoothsteps <- FirstSmoothSteps
  sn_ratio <- FirstSmoothsnratio
 }
 else {sn_ratio <- SecondSmoothsnratio}

# %%%%%%%%%%% MEASUREMENT %%%%%%%%%%%%%%%%%

#%%%%% TAKE log HERE - all remaining work done on the log curve! %%%%%%%%%%
smoothcurve <- log2(smoothcurve)

growthparams <- growthparams(smoothcurve,smoothsteps)

########## PARAMETERS TO BE RETURNED ########################
StartValue 	<- growthparams$StartValue			#Value at start of smoothed curve.   In original (linear not log) units.
StartValueTime <-  growthparams$StartValueTime	#Timepoint of StartValue (not necessarily time zero - first point(s) could be discarded as outliers)
MinValue <- growthparams$MinValueLin			#Minimum value of the smoothed curve.  In original (linear not log) units.
MinValueTime <- growthparams$MinValueTime		#Timepoint of MinValueLin
MaxValue <- growthparams$MaxValueLin			#Maximum measured value after smoothing.  In original (linear not log) units.
MaxValueTime <- growthparams$MaxValueTime       #Timepoint of the first occurrence of the maximum measured value
EndValue <- growthparams$EndValue				#Value at end point of smoothed curve. In original (linear not log) units.
EndValueTime <- growthparams$EndValueTime 		#Timepoint of EndValue
lagtime <- growthparams$lagtime                 #Time taken for growth rate to become greater than 1% of the maximum growth rate (inverse of linearslope).
miylagtime <- growthparams$miylagtime           #Timepoint where backward extrapolation of the linear growth region becomes equal to the minimum of the curve
startlinear <- growthparams$startlinear         #Start timepoint of 'linear' growth (in log space, equivalent to exponential growth in linear space)
endlinear <- growthparams$endlinear             #End point ....  ditto ...
linearslope <- growthparams$linearslope         #The slope of the 'linear' part of growth (in log space) expressed as the linear slope of the log curve
doubletime <- growthparams$doubletime           #Length of time taken for the measured value to double, in the maximum growth phase
dur_linear <- endlinear-startlinear             #Duration of the 'linear' growth region. Is (endlinear-startlinear).
snratio <- sn_ratio								#Signal to noise ratio: indicates how well the curve and the parameters fit the data. Higher is better.

### The following two are available (mainly for the plotting below) but are not required to be returned.
slopes <- growthparams$slopes                   #The first time derivative of the growth curve (in log space).
thresh <- growthparams$thresh                   #The threshold on the first derivative curve used to determine the region of 'linear' growth (in log space)

outparams<-list(StartValue=StartValue,StartValueTime=StartValueTime,MinValue=MinValue,MinValueTime=MinValueTime,MaxValue=MaxValue,MaxValueTime=MaxValueTime,EndValue=EndValue,EndValueTime=EndValueTime,lagtime=lagtime,miylagtime=miylagtime,startlinear=startlinear,endlinear=endlinear,linearslope=linearslope,doubletime=doubletime,dur_linear=dur_linear,snratio=snratio)  

################# ONLY DIAGNOSTIC PLOTTING FROM HERE ONWARDS - added jjr 2/12/09 ##################################
if (plots == "PLOTSON")
{graphics.off() #close any previous plots
 #open graphics windows
     #quartz("plot2")
     #quartz("plot3")
     #quartz("plot4")
     #quartz("plot5")
     #quartz("plot6")
     #quartz("plot7")
     #quartz("plot8")  ###Commented out jjr 11/5/10

#dev.set(2)
dev.new()
plot(filledtsteps,filledcurve,main = "FirstSmooth and filledcurve")
lines(FirstSmoothSteps,FirstSmoothCurve,col = "magenta")
 
#dev.set(3)
dev.new()
plot(origtsteps,origcurve,col="pink",main = "black is cleaned data, pink orig")
points(FirstSmoothSteps,FirstSmoothCurve,col="green")
points(cleanTsteps,cleancurve,pch=20,col="black")

#dev.set(4)
dev.new() 
plot(filledtsteps,filledcurve,col = "magenta", main = title)
###points(FullCurveT,unifcurveOD,col = "green")
lines(FirstSmoothSteps,FirstSmoothCurve,col = "blue")

#dev.set(5)
dev.new()
plot(filledtsteps,log2(filledcurve),col = "magenta", main = c(title,"red is final fit"))
lines(CleanCurve$Tsteps,log2(CleanCurve$cleancurve),col = "red")
lines(FirstSmoothSteps,log2(FirstSmoothCurve),col = "green")
lines(smoothsteps,smoothcurve,col = "black")
#still need to add the remainder to this plot
bottom <- (min(log2(filledcurve)))
top <- (max(log2(filledcurve)))
lines(c(outparams$endlinear,outparams$endlinear),c(bottom,top))
lines(c(outparams$startlinear,outparams$startlinear),c(bottom,top))

#dev.set(6)
#plot(timepointsout,slopes,col = "black", main = title)
#lines(timepointsout,slopes,col = "black")
#p <- c(0,max(timepointsout))
#q <- c(thresh,thresh)
#lines(p,q,col = "black")
#Still need to add this:  ,[0 max(tstepsout)],[thresh thresh],'k--',startlinear,thresh,'k*',endlinear,thresh,'k*'))

# slopes curve - temporarily commented out
dev.new()
plot(smoothsteps,slopes,col = "black", main = title)
lines(smoothsteps,slopes,col = "black")
p <- c(0,max(smoothsteps))
q <- c(thresh,thresh)
lines(p,q,col = "black")

#dev.set(7)   ###Commented out jjr 11/5/10
#plot(truncTsteps,log2(trunccurve),col = "magenta", main = title)
#plot(smoothsteps,log2(origcurve),col = "magenta", main = title)
#lines(smoothsteps,log2curve, col = "black")
#lines(smoothsteps,smoothcurve, col = "black")
}
return(outparams)
}

