\name{xcms2method}
\alias{xcms2method}
\alias{peaklist2picklist}
\alias{picklist2method}
\alias{picklists2methods}
\alias{picklist2Agilent}
\alias{picklist2waters}
\alias{picklists2waters}

\title{
Convenience method to go from XCMS peak tables or diffreports
straight to Bruker micrOTOF control method files
}
\description{
  This convenience method will create (a set of) Bruker method files
  from a template, filling it with tandem mass spectrometry target
  segments for the "interesting" peaks in the peak list  
}
\usage{
peaklist2picklist(anyPeaklist, gradientStart = NULL, gradientEnd = NULL,
widthFactor = 1, minWidth = 1, fillGaps=TRUE)

xcms2method(peaklist, methodPrefix = "",
            MSmode = c("positive", "negative"),
            template = "test.m/microTOFQAcquisition.method",
            gradientStart = NULL, gradientEnd = NULL,
            widthFactor = 1, minWidth = 1,
            MSMSManual_ListCollisionEnergy = 15,
            MSMSManual_ListIsolationWidth = 8)

picklist2method(pickList, methodPrefix = "",
                MSmode = c("positive","negative"),
                template = "test.m/microTOFQAcquisition.method",
                MSMSManual_ListCollisionEnergy = 15,
                MSMSManual_ListIsolationWidth = 8,
                instrumentprefix = "qtofacq")

picklists2methods(pickLists, methodPrefix = "", ...)

picklist2Agilent(pickList, filename="TargetedMSMSTable.csv", 
                 delta_ret_time=10,
                 iso_width=c("Narrow (~1.3 m/z)", "Medium (~4 m/z)"),
                 MSMSManual_ListCollisionEnergy = 20)

picklist2waters(pickList,
                methodPrefix="",
                MSmode=c("positive","negative"),
                template=NULL,
                MSMSManual_ListCollisionEnergy=15,
                TOFConeVoltage=NULL,
                lockMassMZ=NULL)

picklists2waters(pickLists, methodPrefix="", ...)

}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{peaklist}{Eith a peak list with columns rtmin/rt/rtmax and
    mzmin/mz/mzmax or a diffreport.
}
  \item{anyPeaklist}{Eith a peak list with columns rtmin/rt/rtmax and
    mzmin/mz/mzmax or a diffreport.
}
  \item{pickList}{
    A picklist is XXX.
}
  \item{pickLists}{
    A list of pickLists.
}
  \item{methodPrefix}{
    Outfile prefix for the created method(s)
  }
  \item{filename}{
    Outfilename for the created method
  }
  
  \item{MSmode}{
    Polarity of the requested tandem MS measurement
}
  \item{template}{
    A Bruker *.m template file
}
  \item{gradientStart}{
    if present, only use peaks later than this minimum retention time
    (in seconds)
}
  \item{gradientEnd}{
    if present, only use peaks before this maximum retention time
    (in seconds)
}
  \item{widthFactor}{
    enlarge the precursor peaks by this factor for the target windows
}
  \item{minWidth}{
    Ensure a minimum retention time window for the target windows
}
  \item{fillGaps}{
    If set to TRUE, then adjacent segments will be enlarged so that they
    meet "in the middle", otherwise if FALSE then rtmin and rtmax are unchanged
}

\item{MSMSManual_ListCollisionEnergy}{
    Specify one or more collision energies to use
}
  \item{MSMSManual_ListIsolationWidth}{
    Specify one or more isolation widths to use
  }

  \item{iso_width}{
    For Agilent, the requested Isolation width specification. 
  }
  \item{delta_ret_time}{
    For Agilent.
  }
  
  \item{instrumentprefix}{
  The XML tags inside the Bruker method files depend on the instrument,
  so for the micrOTOFq use "qtofacq", or for the maXis "qtofmaxacq".
}

\item{TOFConeVoltage}{
  Specific to Waters
}
\item{lockMassMZ}{
  Specific to Waters
}


\item{...}{
    The picklists2something() wrapper methods will pass the
    ... arguments to the corresponding picklist2something() method.
}

}
\details{
  Prioritizes by what ?
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

\section{Warning }{
  The created Bruker control files are not guaranteed to work on your
  setup, and incorrect control files could damage equipment.   
}

\seealso{  
  \code{\link{plotMS2windows}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
