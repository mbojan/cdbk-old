\name{varFreq}
\docType{methods}
\alias{varFreq}
\alias{varFreq-methods}
\alias{varFreq,cdbkVector-method}
\title{Variable frequency table}
\description{Compute variable frequency table with counts and frequencies,
including NAs.}
\usage{
varFreq(object, ...)
}
\arguments{
  \item{object}{R object}
  \item{\dots}{other arguments passed to other methods}
\details{
This is a generic function currently with method for objects of class
"\linkS4class{cdbkVector}".
}
\value{
The function returns objects of class "\linkS4class{varFreq}" which
in turn has \code{\link{show}} and other "printing" methods defined.
}
\seealso{
Classes "\linkS4class{varFreq}", "\linkS4class{cdbkVar}"
}
