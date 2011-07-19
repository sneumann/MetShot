\name{picklist2method}
\alias{picklist2method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
picklist2method(pickList, method = "", MSmode = c("positive", "negative"), template = "test.method", MSMSManual_ListCollisionEnergy = 15, MSMSManual_ListIsolationWidth = 8)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pickList}{
%%     ~~Describe \code{pickList} here~~
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
function(pickList, method="", MSmode=c("positive","negative"),
                        template="test.method",
                        MSMSManual_ListCollisionEnergy=15,
                        MSMSManual_ListIsolationWidth=8)
{
    require(XML) || stop("Couldn't load package XML")


  ##
  ## Load and parse Template
  ##

  tree <-  xmlTreeParse(paste(template, "microTOFQAcquisition.method", sep="/"))
  root <-  xmlRoot(tree)

  ##
  ## Create a new timetable and add the first segment
  ## which has all the machine settings
  ##

  newTable <- xmlNode("timetable")
##   firstSegment <- root[["method"]][["qtofacq"]][["timetable"]][[1]]
##   newTable <- addChildren(newTable, firstSegment)

  ## Second Segment has to be the first MRM in the template
  segmentTemplate <- root[["method"]][["qtofacq"]][["timetable"]][[2]]

  newSegment <- segmentTemplate
  dependentNr <- 2

##    if (MSmode=="positive") {
##      if (xmlAttrs(newSegment[[dependentNr]])["polarity"] != "positive")
##          stop(paste("Polarity ", MSmode, "at wrong position in template"))
##    } else {
##      if (xmlAttrs(newSegment[[dependentNr]])["polarity"] != "positive")
##          stop(paste("Polarity ", MSmode, "at wrong position in template"))
##    }



  ##
  ## Some sanity checks
  ##


  if ("3.000000" != value4attribute(newSegment, "Mode_ScanMode")) {
      stop("Second Segment in Template file is not MRM.")
  }

  if (length(newSegment[[dependentNr]][[1]])>1)
    warning("More than one parent mass in template")


  posMSMSManual_ListIsolationMass <- index4attribute(newSegment[[dependentNr]],
                                                     "MSMSManual_ListIsolationMass")
  posMSMSManual_ListIsolationWidth <- index4attribute(newSegment[[dependentNr]],
                                                      "MSMSManual_ListIsolationWidth")
  posMSMSManual_ListCollisionEnergy <- index4attribute(newSegment[[dependentNr]],
                                                       "MSMSManual_ListCollisionEnergy")

  if (is.null(posMSMSManual_ListIsolationMass))
      stop("Template file corrupt: MSMSManual_ListIsolationMass not here.")
  if (is.null(posMSMSManual_ListIsolationWidth))
      stop("Template file corrupt: MSMSManual_ListIsolationWidth not here.")
  if (is.null(posMSMSManual_ListCollisionEnergy))
      stop("Template file corrupt: MSMSManual_ListCollisionEnergy not here.")

  ##
  ## Create the new segments
  ##


  for (i in 1:nrow(pickList)) {
    newSegment <- segmentTemplate
    newSegment <- removeAttributes(newSegment, "endtime")
    newSegment <- addAttributes(newSegment,
                                .attrs=c(endtime = pickList[i,"rtmax"]/60))

    newSegment[[dependentNr]][[posMSMSManual_ListIsolationMass]][[1]] <- xmlNode("entry_double",
                                                   attrs=c(value=pickList[i,"mzmed"]))

    newSegment[[dependentNr]][[posMSMSManual_ListIsolationWidth]][[1]] <- xmlNode("entry_double",
                                                    attrs=c(value=MSMSManual_ListIsolationWidth))

    newSegment[[dependentNr]][[posMSMSManual_ListCollisionEnergy]][[1]] <- xmlNode("entry_double",
                                                    attrs=c(value=MSMSManual_ListCollisionEnergy))

    ## Add this segment to the table
    newTable <- addChildren(newTable, newSegment)
  }

  ## Manuallz Add the Calibration Segment
  newSegment <- root[["method"]][["qtofacq"]][["timetable"]][[3]]
  newSegment <- removeAttributes(newSegment, "endtime")
  newSegment <- addAttributes(newSegment, .attrs=c(endtime = "18"))
  newTable <- addChildren(newTable, newSegment)

  newTable <- addChildren(newTable, root[["method"]][["qtofacq"]][["timetable"]][[3]])
  newTable <- addChildren(newTable, root[["method"]][["qtofacq"]][["timetable"]][[4]])

  root[["method"]][["qtofacq"]][["timetable"]] <- newTable

  ##
  ## Copy the template directory
  ## and inject the new method
  ##

    system(paste ("rm -rf ", method))
    system(paste ("cp -ax ", template, method))


  saveXML(root, file=paste(method, "microTOFQAcquisition.method", sep="/"))

  invisible(pickList[,,drop=FALSE])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
