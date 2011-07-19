\name{plotMS2windows}
\alias{plotMS2windows}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotMS2windows(anyPeaklist, pickList, peaks = TRUE, col = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{anyPeaklist}{
%%     ~~Describe \code{anyPeaklist} here~~
}
  \item{pickList}{
%%     ~~Describe \code{pickList} here~~
}
  \item{peaks}{
%%     ~~Describe \code{peaks} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
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
