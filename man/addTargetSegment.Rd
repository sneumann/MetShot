\name{addTargetSegment}
\alias{addTargetSegment}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
addTargetSegment(traml, MSmode = c("positive", "negative"), MSMSManual_ListCollisionEnergy = 15, MSMSManual_ListIsolationWidth = 8)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{traml}{
%%     ~~Describe \code{traml} here~~
}
  \item{MSmode}{
%%     ~~Describe \code{MSmode} here~~
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
function(traml,
                             MSmode=c("positive","negative"),
                             MSMSManual_ListCollisionEnergy=15,
                             MSMSManual_ListIsolationWidth=8) {
  
  traml$addNode("Target", attrs=c(
                            id=7
                            ##, compoundRef="T108M381"
                            ), close=FALSE)
  traml$addNode("Precursor", close=FALSE)
  traml$addNode("cvParam",
                attrs=c(
                  cvRef="MS",
                  accession="MS:1000827",
                  name="isolation window target m/z",
                  value="4.0",
                  unitCvRef="MS",
                  unitAccession="MS:1000040",
                  unitName="m/z"))
  traml$closeTag() ## Precursor

  traml$addNode("RetentionTime", close=FALSE)
  traml$addNode("cvParam",
                attrs=c(
                  cvRef="MS",
                  accession="MS:1000895",
                  name="local retention time",
                  value="5.0",
                  unitCvRef="UO",
                  unitAccession="UO:0000010",
                  unitName="second"))
  traml$addNode("cvParam",
                attrs=c(
                  cvRef="MS",
                  accession="MS:1000916",
                  name="retention time window lower offset",
                  value="6.0",
                  unitCvRef="UO",
                  unitAccession="UO:0000010",
                  unitName="second"))
  traml$addNode("cvParam",
                attrs=c(
                  cvRef="MS",
                  accession="MS:1000917",
                  name="retention time window upper offset",
                  value="7.0",
                  unitCvRef="UO",
                  unitAccession="UO:0000010",
                  unitName="second"))
    traml$closeTag() ## RetentionTime

  traml$addNode("ConfigurationList", close=FALSE)
  traml$addNode("Configuration",
                attrs=c(instrumentRef="microtof"),
                close=FALSE)
  traml$addNode("cvParam",
                attrs=c(
                  cvRef="MS",
                  accession="MS:1000045",
                  name="collision energy",
                  value="8.0",
                  unitCvRef="UO",
                  unitAccession="UO:0000266",
                  unitName="electronvolt"))
  traml$closeTag() ## Configuration
  traml$closeTag() ## ConfigurationList  
  traml$closeTag() ## Target

  traml  
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
