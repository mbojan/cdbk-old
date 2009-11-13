\name{varTech-class}
\docType{class}
\alias{varTech-class}
\alias{show,varTech-method}

\title{Object for technical variable summary}
\description{Objects of this class contain simple "technical"
summary information of a variable.}
\section{Objects from the Class}{
Objects can be created by generic \code{\link{varTech}}.
}
\section{Slots}{
  \describe{
    \item{\code{varname}:}{character, name of the variable}
    \item{\code{varclass}:}{character, variable class, as returned by
    \code{\link{class}}}
    \item{\code{varmode}:}{character, storage mode of the variable as
      returned by \code{\link{mode}}}
    \item{\code{hasnames}:}{logical, does the object has \code{names}
      attribute defined}
    \item{\code{uvalues}:}{numeric, number of unique values}
    \item{\code{nas}:}{numeric, number of \code{NA}s}
    \item{\code{size}:}{numeric, size of the variable in bytes}
  }
}
\section{Extends}{
Class \code{"\linkS4class{cdbkVar}"}, directly.
Class \code{"\linkS4class{cdbkObject}"}, by class "cdbkVar", distance 2.
}
\section{Methods}{
  \describe{
    \item{prettyMatrix}{\code{signature(object = "varTech")}: ... }
    \item{show}{\code{signature(object = "varTech")}: ... }
	 }
}
\seealso{
Constructor for this class \code{\link{varTech}}.
}
\examples{
showClass("varTech")
}
