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
  This function combines several low-level plots.
  
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
Steffen Neumann
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
