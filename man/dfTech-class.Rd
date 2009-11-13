\name{dfTech-class}
\docType{class}
\alias{dfTech-class}
\alias{show,dfTech-method}

\title{Technical information about a dataset}
\description{Objects of this class contain data-level summary
information. It is rather technical.}
\section{Objects from the Class}{
Objects can be created by calls to \code{\link{dfTech}} generic.
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
Class \code{"\linkS4class{cdbkDf}"}, directly.
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "cdbkDf")}: Display the content of the
    object. }
	 }
}
\seealso{
Constructor \code{\link{dfTech}} to create objects of this class as well as
some examples.
}
