\name{cdbkDf-class}
\docType{class}
\alias{cdbkDf-class}
\alias{show,cdbkDf-method}

\title{Data-level documentation objects}
\description{Objects of this class contain data-level summary
information.}
\section{Objects from the Class}{
Objects can be created by calls to \code{\link{cdbkDf}} generic.
}
\section{Slots}{
	 \describe{
    \item{\code{ncases}:}{numeric, number of cases}
    \item{\code{nvars}:}{numeric, number of variables}
    \item{\code{ncomplete}:}{numeric, number of complete cases}
    \item{\code{size}:}{numeric, size of the data object in bytes}
  }
}
\section{Extends}{
Class \code{"\linkS4class{cdbkObject}"}, directly.
}
\section{Methods}{
  \describe{
    \item{cdbkData}{\code{signature(dat = "cdbkDf", vars = "list")}: ... }
    \item{show}{\code{signature(object = "cdbkDf")}: ... }
	 }
}
\seealso{
Constructor \code{\link{cdbkDf}} to create objects of this class as well as
some examples.
}
