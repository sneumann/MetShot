\name{beginTraml}
\alias{beginTraml}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
beginTraml()
}
%- maybe also 'usage' for other objects documented here.
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
function() { 
  tramlVersion="0.9.4"
  schemaLocation="http://psi.hupo.org/ms/traml TraML0.9.4.xsd"
  
  ##
  ## TraML Root
  ##
  
  traml = xmlTree(tag="TraML",
    attrs=c(
      version=tramlVersion,
      "xsi:schemaLocation"=schemaLocation),
    namespaces = c(
      "http://psi.hupo.org/ms/traml",
      xsi="http://www.w3.org/2001/XMLSchema-instance")
    )
  
  ##
  ## CV List
  ##

  traml$addNode("cvList", close = FALSE)
  traml$addNode("cv", attrs=c(
                        id="MS",
                        fullName="Proteomics Standards Initiative Mass Spectrometry Ontology",
                        version="2.29.0",
                        URI="http://psidev.cvs.sourceforge.net/*checkout*/psidev/psi/psi-ms/mzML/controlledVocabulary/psi-ms.obo"))
  traml$addNode("cv", attrs=c(
                        id="UO",
                        fullName="Unit Ontology",
                        version="1.20",
                        URI="http://obo.cvs.sourceforge.net/obo/obo/ontology/phenotype/unit.obo"))
  traml$closeTag()

  ##
  ## InstrumentList
  ##

  traml$addNode("InstrumentList", close = FALSE)
  traml$addNode("Instrument",
                attrs=c(id="microtof"),
                close = FALSE)
  traml$addNode("cvParam", attrs=c(
                             cvRef="MS",
                             accession="MS:1001536",
                             name="Bruker Daltonics micrOTOF series"
                             ))
  traml$closeTag() ## Instrument
  traml$closeTag() ## InstrumentList

  
  ##
  ## SoftwareList
  ##

  traml$addNode("SoftwareList", close = FALSE)
  traml$addNode("Software",
                attrs=c(
                  id="XCMS-1.12",
                  version=packageDescription("xcms", fields="Version", drop=TRUE)),                
                close = FALSE)

  ##   traml$addNode("cvParam", attrs=c(
  ##                              cvRef="MS",
  ##                              accession="MS:1000xxx",
  ##                              name="XCMS",
  ##                              version=packageDescription("xcms", fields="Version", drop=TRUE)
  traml$addNode("cvParam", attrs=c(
                             cvRef="MS",
                             accession="MS:1001582",
                             name="XCMS"))
  traml$closeTag()
  traml$closeTag()

  ##
  ## TargetList
  ##

  traml$addNode("TargetList", close = FALSE)
  traml$addNode("cvParam", attrs=c(
                             cvRef="MS",
                             accession="MS:1000920",
                             name="includes supersede excludes"))

  traml$addNode("TargetIncludeList", close = FALSE)

  traml
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
