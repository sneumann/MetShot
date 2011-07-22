## plotXFPeaks <-
## function(xf, xr=NULL, title=NULL) {

##     ms1peaks <- which(xf@peaks[,"msLevel"]==1)
##     ms2peaks <- which(xf@peaks[,"msLevel"]==2)

##     mzrange <- range(xf@peaks[,"mz"])

##     if (! is.null(xr)) {
##         ## fake a mass=0
##         xrr <- makeMiniXR(xr)
##         xrr@env$mz <- c(xrr@env$mz, 0)
##         xrr@env$intensity <- c(xrr@env$intensity, 0)
##         xrr@scanindex[-1] <- as.integer(xrr@scanindex[-1]+1)
##         plotRaw(xrr, log=TRUE, title=title)
##         points (xf@peaks[ms1peaks,c("rt","mz")], type="h", col="grey")
##     } else {
##         plot (xf@peaks[ms1peaks,c("rt","mz")], type="h", col="grey",
##               ylim=mzrange, title=title)
##     }
##     points (xf@peaks[ms1peaks,c("rt","mz")], col="red")

##     require(gstat)
##     colorlut <- bpy.colors(16)
##     ints <- log(xf@peaks[ms2peaks,"intensity"])

##     points (xf@peaks[,c("rt","mz")], pch=".", cex=4, col=colorlut[ints/max(ints)*15+1])

## }

