\name{findWindow}
\alias{findWindow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
findWindow(peaklist, rtmin, rtmax)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{peaklist}{
%%     ~~Describe \code{peaklist} here~~
}
  \item{rtmin}{
%%     ~~Describe \code{rtmin} here~~
}
  \item{rtmax}{
%%     ~~Describe \code{rtmax} here~~
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
function(peaklist, rtmin, rtmax) {

  ##
  ## Prefilter peaks between window of interest
  ##
  inWindow <-    which(  peaklist[,"rtmin"] >= rtmin
                       & peaklist[,"rtmax"] >  rtmin
                       & peaklist[,"rtmax"] <= rtmax
                       & peaklist[,"rtmin"] <  rtmax)

  if (length(inWindow) == 0) {
      ## recursion end
      return (NULL);
  }

  ##
  ## Select Peak to Pick and replace min/max
  ##

  center <- max(1,floor(length(inWindow)/2))
  peak <-    peaklist[inWindow[center], ];

##   cat(paste("Using peak", rownames(peaklist)[inWindow[center]],
##              "from", peak["rtmin"],
##              "to", peak["rtmax"],
##             "\n"))

  wl <- findWindow(peaklist,
                   min(rtmin, peak["rtmin"]),
                   max(rtmin, peak["rtmin"]))

  wr <- findWindow(peaklist,
                   min(rtmax, peak["rtmax"]),
                   max(rtmax, peak["rtmax"]))

  newpeak <- rbind(peak)
  rownames(newpeak) <- rownames(peaklist)[inWindow[center]]

  return (as.matrix(rbind(newpeak, wl, wr), ncol=8))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
