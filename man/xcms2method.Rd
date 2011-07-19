\name{xcms2method}
\alias{xcms2method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
xcms2method(peaklist, method = "", MSmode = c("positive", "negative"), template = "test.method", gradientStart = NULL, gradientEnd = NULL, widthFactor = 1, minWidth = 1, MSMSManual_ListCollisionEnergy = 15, MSMSManual_ListIsolationWidth = 8)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{peaklist}{
%%     ~~Describe \code{peaklist} here~~
}
  \item{method}{
%%     ~~Describe \code{method} here~~
}
  \item{MSmode}{
%%     ~~Describe \code{MSmode} here~~
}
  \item{template}{
%%     ~~Describe \code{template} here~~
}
  \item{gradientStart}{
%%     ~~Describe \code{gradientStart} here~~
}
  \item{gradientEnd}{
%%     ~~Describe \code{gradientEnd} here~~
}
  \item{widthFactor}{
%%     ~~Describe \code{widthFactor} here~~
}
  \item{minWidth}{
%%     ~~Describe \code{minWidth} here~~
}
  \item{MSMSManual_ListCollisionEnergy}{
%%     ~~Describe \code{MSMSManual_ListCollisionEnergy} here~~
}
  \item{MSMSManual_ListIsolationWidth}{
%%     ~~Describe \code{MSMSManual_ListIsolationWidth} here~~
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
function(peaklist, method="", MSmode=c("positive","negative"),
                        template="test.method",
                        gradientStart=NULL, gradientEnd=NULL,
                        widthFactor=1, minWidth=1,
                        MSMSManual_ListCollisionEnergy=15,
                        MSMSManual_ListIsolationWidth=8)
{


    pickList <- peaklist2picklist(peaklist,
                                  gradientStart=gradientStart, gradientEnd=gradientEnd,
                                  widthFactor=widthFactor,
                                  minWidth=minWidth)

    if (is.null(pickList)) {
        warning("No suitable peaks found, returning empty picklist")
        return(NULL)
    }

    picklist2method(pickList, method, MSmode, template,
                    MSMSManual_ListCollisionEnergy=MSMSManual_ListCollisionEnergy,
                    MSMSManual_ListIsolationWidth=MSMSManual_ListIsolationWidth)    
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
