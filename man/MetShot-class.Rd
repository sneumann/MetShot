\name{MetShot-class}
\Rdversion{1.1}
\docType{class}
\alias{MetShot-class}

\title{Class \code{"MetShot"}}
\description{
Keep details about a MetShot run
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("MetShot", versions, ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{peaklist}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{peaknames}:}{Object of class \code{"character"} ~~ }
    \item{\code{picklists}:}{Object of class \code{"list"} ~~ }
    \item{\code{pickNames}:}{Object of class \code{"character"} ~~ }
    \item{\code{schedParams}:}{Object of class \code{"list"} ~~ }
    \item{\code{.__classVersion__}:}{Object of class \code{"Versions"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{Versioned}"}, directly.
}
\section{Methods}{
No methods defined with class "MetShot" in the signature.
}
\author{
Steffen Neumann <sneumann@ipb-halle.de>
}
\examples{
showClass("MetShot")
}
\keyword{classes}
