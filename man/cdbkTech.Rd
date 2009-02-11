\name{cdbkTech}
\alias{cdbkTech}
\docType{methods}
\alias{cdbkTech-methods}
\alias{cdbkTech,cdbkVector-method}
\title{Variable summary: simple technical information}
\description{
Creates a simple "technical" summary of the variable.
}
\usage{
cdbkTech(object, ...)
}
\arguments{
  \item{object}{vector, basic \R\ classes are supported, see
  \linkS4class{cdbkVector}, and Methods below}
  \item{\dots}{other arguments, currently ignored}
}
\details{
This function produces object of class \linkS4class{cdbkTech}
which contains basic technical description of a variable.
See the description of the class.
}
\section{Methods}{
\describe{

\item{object = "cdbkVector"}{The method computes the following information
of the variable supplied in \code{object}:
  \itemize{
    \item vector class, as returned by \code{\link{class}}
    \item vector mode, as returned by \code{\link{mode}}
    \item whether vector has \code{names} attribute defined
    \item number of unique values
    \item number of \code{NA}s
    \item size of the objects in bytes
  }
}}
}
\value{
Object of class \linkS4class{cdbkTech}.
}
\seealso{Description of the class \linkS4class{cdbkTech}}
\examples{
# TODO add examples
}
