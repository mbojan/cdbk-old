\name{prettyMatrix}
\docType{methods}
\alias{prettyMatrix-methods}
\alias{prettyMatrix,cdbkTech-method}
\alias{prettyMatrix}
\title{Helper function for setting-up object printing}
\description{
Creates a matrix-like layout of information for pretty-printing.
}
\usage{
prettyMatrix(object)
}
\arguments{
  \item{object}{object to be processed, see below for available methods}
}
\section{Methods}{
\describe{

\item{object = "cdbkTech"}{The information from the object is set-up in a
3-by-4 matrix such that columns 1 and 3 contain labels and columns 2 an 4
contain data.}
}}
\value{
A character matrix containing the information from the \code{object}.
}
\examples{
# TODO add examples
}
