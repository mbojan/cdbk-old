\name{cdbkUtils}
\alias{txtPrettyRow}
\title{Package utility functions}
\description{
Small helper functions of package \pkg{cdbk}.
}
\usage{
txtPrettyRow(r, sep = " ")
}
\arguments{
  \item{r}{character vector, usually row of a matrix created by
  \code{\link{prettyMatrix}}}
  \item{sep}{character, separator passed to \code{\link{paste}}}
}
\details{
\code{txtPrettyRow} Given a row of the pretty matrix collapse labels with
values and return the pasted string. It is used for pretty printing by for
example by \code{\link{cdbkTxt}} methos.
}
\value{
For \code{txtPrettyRow} a character string (vector of length 1).
}
\seealso{\code{\link{cdbkTxt}}, \code{\link{prettyMatrix}}}
\examples{
# TODO add examples
}
