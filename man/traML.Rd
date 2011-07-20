\name{traML}
\alias{beginTraml}
\alias{addTargetSegment}
\alias{endTraml}
\title{
  Creating traML files from pickLists
}
\description{
    Create traML objects, and add segments describing the parameters for
  tandem MS spectra.
}
\usage{
beginTraml()

addTargetSegment(traml, MSmode = c("positive", "negative"), MSMSManual_ListCollisionEnergy = 15, MSMSManual_ListIsolationWidth = 8)

endTraml(traml)
}
\arguments{
  \item{traml}{
    An traML XML document
}
  \item{MSmode}{
    Polarity of the requested tandem MS segment
}
  \item{MSMSManual_ListCollisionEnergy}{
    Specify the collision energy to use
}
  \item{MSMSManual_ListIsolationWidth}{
    Specify the isolation width to use
}
}
\details{
  Currently will add a predefined segment.
}
\value{
  Return an XML document with a new segment  
}
\references{
  http://www.psidev.info/index.php?q=node/405
}
\author{
  Steffen Neumann
}

\seealso{
  beginTraml, endTraml, picklist2traml
}
\examples{
traml <- NULL
traml <- beginTraml()
traml <- addTargetSegment(traml)
traml <- endTraml(traml)
}
\keyword{XML}
\keyword{traML}
