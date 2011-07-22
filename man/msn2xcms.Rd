\name{msn2xcms}
\alias{msn2xcms}
\title{
  Helper function to create a normal xcmsRaw from MSn data
}
\description{
  Helper function to create a normal xcmsRaw from MSn data.
  Copy xrawMSn@env$msnmz and xrawMSn@env$msnintensities and
  all metadata from xrawMSn to xraw.
}
\usage{
msn2xcms(xmsn)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xmsn}{
%%     ~~Describe \code{xmsn} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  An xcmsRaw object, where the converted MSn data appears as
  normal MS1 data. This can be passed to findPeaks().  
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
function(xmsn) {
    x <- xmsn
                                        # BROKEN
    x@tic <- x@msnAcquisitionNum
                                        # Fake time in secs
    x@scantime <- x@msnRt
    x@acquisitionNum <- x@msnAcquisitionNum
    x@scanindex <- x@msnScanindex

    x@env$mz <- x@env$msnMz
    x@env$intensity <- x@env$msnIntensity
    invisible(x)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
