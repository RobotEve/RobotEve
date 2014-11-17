zespline <- function(x,y,xx){
	###Need to add 4 points at each end to force zero slope
	Xmeangap <- (max(x)-min(x))/(length(x)-1)
	Yext <- c(y[1],y[1],y[1],y[1],y,y[length(x)],y[length(x)],y[length(x)],y[length(x)])
	Xext <- c(min(x)-4*Xmeangap,min(x)-3*Xmeangap,min(x)-2*Xmeangap,min(x)-Xmeangap,x,max(x)+Xmeangap,max(x)+2*Xmeangap,max(x)+3*Xmeangap,max(x)+4*Xmeangap)
	XXmeangap <- (max(xx)-min(xx))/(length(xx)-1)
	XXext <- c(min(xx)-4*Xmeangap,min(xx)-3*Xmeangap,min(xx)-2*Xmeangap,min(xx)-Xmeangap,xx,max(xx)+Xmeangap,max(xx)+2*Xmeangap,max(xx)+3*Xmeangap,max(xx)+4*Xmeangap)
	newxy <- spline(Xext,Yext,n = 3*length(Xext), method = "fmm", xmin = min(Xext), xmax = max(Xext),XXext, ties = mean) #n must be specified but is not used if xout is specified, as here (XXext)
	#NOW NEED TO TRUNCATE BEFORE RETURNING
	L <- length(newxy$x)
	newxy$x <- newxy$x[5:(L-4)] #should restore original timespan by removing the 4 added points at each end
	newxy$y <- newxy$y[5:(L-4)] #should restore original timespan by removing the 4 added points at each end
	
	return(newxy)
	}
