\name{peaklist2picklist}
\alias{peaklist2picklist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
peaklist2picklist(anyPeaklist, gradientStart = NULL, gradientEnd = NULL, widthFactor = 1, minWidth = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{anyPeaklist}{
%%     ~~Describe \code{anyPeaklist} here~~
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
function(anyPeaklist,
                              gradientStart=NULL, gradientEnd=NULL,
                              widthFactor=1, minWidth=1) {

  ##
  ## handle peaks(xs), groups(xs), diffreport()
  ##

  if (typeof(anyPeaklist)=="list") {
      peaklist <- as.matrix(anyPeaklist[,c("mzmin","mzmed", "mzmax","rtmin","rtmed", "rtmax")])
  } else {
    peaklist <- anyPeaklist
    rownames(peaklist) <- seq(1,nrow(peaklist))
  }

  ##
  ## For a "manual" list without rtmin/rtmax
  ## fake those
  ##

  if (! "rtmin" \%in\% colnames(peaklist) ) {
      peaklist <- cbind(peaklist, rtmin=peaklist[,"rtmed"])
  }
  if (! "rtmax" \%in\% colnames(peaklist) ) {
      peaklist <- cbind(peaklist, rtmax=peaklist[,"rtmed"])
  }

  ##
  ## Recalculate Begin/End of peaks
  ## and forcably expand small peaks
  ##

  ## widen peaks relatively
  widths <- peaklist[,"rtmax"]-peaklist[,"rtmin"]
  newWidths <- pmax(widths * widthFactor, rep(minWidth, length(widths)))

  newMin <- peaklist[, "rtmin", drop=FALSE] - ( newWidths - widths ) / 2
  newMax <- peaklist[, "rtmax", drop=FALSE] + ( newWidths - widths ) / 2


  ##
  ## If no rt range is supplied,
  ## calculate _after_ enlarging
  ##

  if (is.null(gradientStart)) {
    gradientStart <- min(newMin)
  }
  if (is.null(gradientEnd)) {
    gradientEnd <- max(newMax)
  }

  peaklist[, c("rtmin", "rtmax")] <- cbind( rtmin=pmax(rep(gradientStart, length(widths)), newMin),
                                            rtmax=pmin(rep(gradientEnd,   length(widths)), newMax))

  ##
  ## Recursively traverse peaklist
  ##

  o <- order(peaklist[, "rtmed"])
  pickList <- findWindow(peaklist[o,,drop=FALSE],
                         rtmin=gradientStart, rtmax=gradientEnd)

  if (is.null(pickList) || nrow(pickList)<2) {
      return(NULL)
  }

  o <- order(pickList[, "rtmed"])
  pickListOrdered <- pickList[o,,drop=FALSE]
  pickListFilled <- pickListOrdered

  for (i in 1:(nrow(pickListOrdered)-1)) {
      meanTime <- mean(c(pickListOrdered[i,"rtmax"],
                         pickListOrdered[i+1,"rtmin"]))
      pickListFilled[i, "rtmax"] <- meanTime
      pickListFilled[i+1,"rtmin"] <- meanTime
  }

  invisible(pickListFilled)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
