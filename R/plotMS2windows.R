plotMS2windows <-
function(anyPeaklist, pickList,
         peaks=TRUE, col=NULL,
         labels=NULL)
{

    if (is.null(col)) {
        col=4
    }

    ##
    ## handle peaks(xs), groups(xs), diffreport()
    ##

    if (typeof(anyPeaklist)=="list") {
      peaklist <- as.matrix(anyPeaklist[,c("mzmin","mzmed", "mzmax","rtmin","rtmed", "rtmax")])
    } else {
        peaklist <- anyPeaklist
    }

    if (! "rtmin" %in% colnames(peaklist) ) {
        peaklist <- cbind(peaklist, rtmin=peaklist[,"rtmed"])
    }
    if (! "rtmax" %in% colnames(peaklist) ) {
        peaklist <- cbind(peaklist, rtmax=peaklist[,"rtmed"])
    }

    gradientStart <- min(peaklist[,"rtmin"])
    gradientEnd <- max(peaklist[,"rtmax"])
    mzrange <- range(peaklist[,"mzmed"])

    if (peaks){
        plot(rbind(c(gradientStart,mzrange[1]), c(gradientEnd,mzrange[2])),
             xlab="RT [s]", ylab="m/z", type="n")
    }

  ##
  ## Vertical lines
  ##

#  p <- as.matrix(pickList[, c("rtmin", "rtmax", "mzmed", "mzmed")])
#  dim(p) <- c(nrow(p)*2, 2)
#  q <- p[2:(nrow(p)-1), ]
#  dim(q) <- c(nrow(pickList)-1, 4)
#  segments(q[,1], q[,3], q[,2], q[,4], col="blue")

  ##
  ## Horizontal lines
  ##
    segments(pickList[,"rtmin"], pickList[,"mzmed"],
             pickList[,"rtmax"], pickList[,"mzmed"],
             col=col, lwd=2)

    text(pickList[,"rtmin"], pickList[,"mzmed"], labels=labels, col=col, pos=4)

    ## Peak Groups
    segments(peaklist[,"rtmin"], peaklist[,"mzmed"],
             peaklist[,"rtmax"], peaklist[,"mzmed"], col="grey")

}

