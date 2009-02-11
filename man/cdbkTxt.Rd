\name{cdbkTxt}
\docType{methods}
\alias{cdbkTxt}
\alias{cdbkTxt-methods}
\alias{cdbkTxt,cdbkData-method}
\alias{cdbkTxt,cdbkTech-method}
\alias{cdbkTxt,list-method}
\title{Printing objects to plain text format}
\description{
Printing objects to plain text format.
}
\usage{
cdbkTxt(object, ...)
\S4method{cdbkTxt}{cdbkData}(object, file="", append=FALSE,
  title="Dataset documentation")
\S4method{cdbkTxt}{cdbkTech}(object, file="", append=FALSE, ...)
\S4method{cdbkTxt}{list}(object, ...)
}
\arguments{
  \item{object}{\R\ object, see Details and Methods}
  \item{file}{character or a connection to write the documentation to, passed
  to \code{\link{cat}}}
  \item{append}{logical, only used if \code{file} is character naming the file,
  whether to append to a file or start from the beginning}
  \item{title}{character, header for the codebook}
  \item{\dots}{other arguments to/from other methods}
}
\details{
This generic function creates a plain text representation of
the documentation objects.
}
\section{Methods}{
\describe{
\item{object = "cdbkData"}{The method prints the data documentation object.
First the data level information from the object and then proceeds through the
list of the variable objects and prints them subsequently.}

\item{object = "cdbkTech"}{Writes the technical summary of a variable.  It
starts with a header and then prints the rest in tabular form.}

\item{object = "list"}{When called on a list the method is applied to all the
components of the list.}
}}
\value{
Nothing, but the text is printed to a file or console.
}
\seealso{Class description \linkS4class{cdbkTech}}
\examples{
# TODO add examples
}
