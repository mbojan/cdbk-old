\name{genSampleData}
\alias{genSampleData}
\title{Generate example data}
\description{
Generate example data for package \pkg{cdbk}. The resulting data
frame have variables that are numeric, character, factor, and integer
with and without missing data (\code{NA}s).
}
\usage{
genSampleData(n = 20, k = 5, pna = 0.1)
}
\arguments{
  \item{n}{numeric, number of rows in the resulting data frame}
  \item{k}{numeric, number of distinct categories in some of the variables}
  \item{pna}{numeric, probability of \code{NA}s in the variables}
}
\value{
Data frame with columns:
\describe{
  \item{num}{numeric variable}
  \item{ch}{character variable}
  \item{fac}{factor variable}
  \item{int}{integer variable}
  \item{numna}{numeric variable with missing values}
  \item{chna}{character variable with missing values}
  \item{facna}{factor variable with missing values}
  \item{intna}{integer variable with missing values}
}
}
\examples{
set.seed(1234)
d <- genSampleData()
str(d)
}
