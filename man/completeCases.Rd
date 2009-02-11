\name{completeCases}
\alias{completeCases}
\title{Identify complete cases in a data frame}
\description{
Given a data frame return a logical vector indicating
whether the case has any \code{NA}s or not.
}
\usage{
completeCases(d)
}
\arguments{
  \item{d}{data frame}
}
\value{
Logical vector of length \code{nrow(d)}.
}
\seealso{\code{\link{complete.cases}}, why I did not use that anyway?}
\examples{
# TODO add examples
}
