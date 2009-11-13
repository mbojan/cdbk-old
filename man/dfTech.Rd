\name{dfTech}
\alias{dfTech}
\docType{methods}
\alias{dfTech-methods}
\alias{dfTech,data.frame-method}
\title{Constructing data-level documentation}
\description{
This is a generic function for creating objects of class
\linkS4class{dfTech} containing data-level information.
}
\usage{
dfTech(object, ...)
}
\arguments{
  \item{object}{\R\ object, see Details and Methods}
  \item{\dots}{other arguments passed to/from other methods}
}
\details{
This function is a constructor for objects of class \linkS4class{dfTech}.
These objects contain data level information that is usually printed
in the begining of a codebook. See the description of the class
for available slots.
}
\section{Methods}{
\describe{
\item{object = "data.frame"}{The method assembles the object by computing basic
data-level information. This includes the number of cases, number of variables,
number of complete cases, and the size of the data object in bytes (as in \R
workspace).}
}}
\value{
Object of class \linkS4class{dfTech}.
}
\seealso{Description of class \linkS4class{dfTech}}
\examples{
d <- data.frame(x=1:5, y=letters[1:5])
o <- dfTech(d)
o
}
