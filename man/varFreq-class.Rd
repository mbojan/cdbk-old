\name{varFreq-class}
\Rdversion{1.1}
\docType{class}
\alias{varFreq-class}
\alias{show,varFreq-method}
\title{Frequency table}
\description{Class representing an extended frequency table including count and percentage columns.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("varFreq", ...)}.
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
}
\section{Methods}{
  \describe{
    \item{cdbkTxt}{\code{signature(object = "varFreq")}: print the table in plain text}
	 }
}
\examples{
showClass("varFreq")
}
\keyword{classes}
