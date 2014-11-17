#linterp - written jjr 2/12/09, corrected 8/12/09!
#performs linear interpolation between the first and last points
#represented by the x,y pairs, at the new time points xi.
#Primarily intended to take only two x,y points but will work 
#with any number of xy points.

linterp <- function(x,y,xi){
	gapy     <- c(y[1],y[length(y)])
	reqdgaps <- length(xi)+1;
	newygap  <- (gapy[2]-gapy[1])/reqdgaps	 
	newy <- y[1] + ((1:length(xi))*newygap)
	 }
