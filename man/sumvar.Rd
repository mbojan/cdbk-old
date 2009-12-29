\name{sumvar}
\alias{sumvar}
\alias{print.sumvar}
\title{Summary of variables a data frame}
\description{
Brief summary of variables in a data frame. Includes number of unique values,
number of missing values and storage mode. This is a generic function.}
\usage{
sumvar(object, ...)
}
\arguments{
  \item{object}{R object}
  \item{\dots}{other arguments from/to methods}
}
\value{
Object of S3  class \code{sumvar} extending \code{matrix}. The class has
\code{print} method.
}
\examples{
data(testdata)
sumvar(testdata)
}
