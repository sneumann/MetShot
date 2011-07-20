\name{plotMS2windows}
\alias{plotMS2windows}
\title{
	Overlay plot of peak list and generated picklist(s)
}
\description{

	Visualisation of the generated picklists. Because the peak
	lists are superimposed, one can easily see how much instrument
	time is "unused".
}
\usage{
plotMS2windows(anyPeaklist, pickList, peaks = TRUE, col = NULL)
}
\arguments{
  \item{anyPeaklist}{Eith a peak list with columns rtmin/rt/rtmax and
    mzmin/mz/mzmax or a diffreport.
}
  \item{pickList}{
    A picklist is XXX.
}
  \item{peaks}{
  Boolean, whether peaks shall be plotted atop the picklist windows
}
  \item{col}{
  Color for the picklists
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{xcms2method}},
  \code{\link{peaklist2picklist}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(anyPeaklist, pickList,
                           peaks=TRUE, col=NULL)
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

    if (! "rtmin" \%in\% colnames(peaklist) ) {
        peaklist <- cbind(peaklist, rtmin=peaklist[,"rtmed"])
    }
    if (! "rtmax" \%in\% colnames(peaklist) ) {
        peaklist <- cbind(peaklist, rtmax=peaklist[,"rtmed"])
    }

    gradientStart <- min(peaklist[,"rtmin"])
    gradientEnd <- max(peaklist[,"rtmax"])
    mzrange <- range(peaklist[,"mzmed"])

    if (peaks){
        plot(rbind(c(gradientStart,mzrange[1]), c(gradientEnd,mzrange[2])),
             xlab="RT", ylab="m/z", type="n")
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


    ## Peak Groups
    segments(peaklist[,"rtmin"], peaklist[,"mzmed"],
             peaklist[,"rtmax"], peaklist[,"mzmed"], col="grey")

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
