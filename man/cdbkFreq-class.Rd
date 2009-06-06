\name{cdbkFreq-class}
\Rdversion{1.1}
\docType{class}
\alias{cdbkFreq-class}
\alias{cdbkTxt,cdbkFreq-method}

\title{Frequency table}
\description{Class representing an extended frequency table including count and percentage columns.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("cdbkFreq", ...)}.
}
\section{Slots}{
	 \describe{
    \item{\code{varname}:}{character, name of the variable}
    \item{\code{freq}:}{numeric, vector of counts}
    \item{\code{pct}:}{numeric, vector of percentages}
    \item{\code{labels}:}{character, value lables}
  }
}
\section{Extends}{
Class \code{"\linkS4class{cdbkVar}"}, directly.
Class \code{"\linkS4class{cdbkObject}"}, by class "cdbkVar", distance 2.
}
\section{Methods}{
  \describe{
    \item{cdbkTxt}{\code{signature(object = "cdbkFreq")}: print the table in plain text}
	 }
}
\examples{
showClass("cdbkFreq")
}
\keyword{classes}
