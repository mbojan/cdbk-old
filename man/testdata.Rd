\name{testdata}
\alias{testdata}
\docType{data}
\title{Example data}
\description{
An example data frame used for testing.
}
\usage{data(testdata)}
\format{
  A data frame with 50 observations on the following 8 variables.
  \describe{
    \item{\code{num}}{a numeric vector}
    \item{\code{ch}}{a character vector}
    \item{\code{fac}}{a factor with levels \code{a} \code{b} \code{c} \code{d} \code{e}}
    \item{\code{int}}{a numeric vector}
    \item{\code{numna}}{a numeric vector}
    \item{\code{chna}}{a character vector}
    \item{\code{facna}}{a factor with levels \code{a} \code{b} \code{c} \code{d} \code{e}}
    \item{\code{intna}}{a numeric vector}
  }
}
\details{
This data frame is used for testing purposes. It was generated using
\code{\link{genSampleData}}.
}
\seealso{\code{\link{genSampleData}}}
\examples{
data(testdata)
str(testdata)
}
\keyword{datasets}
