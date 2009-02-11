\name{cdbk-package}
\alias{cdbk-package}
\alias{cdbk}
\docType{package}
\title{Package 'cdbk': Automated documentation of datasets}
\description{
This package implement a simple system for producing publication-ready
documentation of a data set (aka codebooks). Implemented functions enable
producing documents that include summary information for all the variables in
the given data. The package supports producing codebooks in plain text, HTML as
well as LaTeX formats. The flexible design of the system enables the user to
define their own ways the data are summarized in the document by supplying
user-programmed classes and methods that extend the currently defined ones.
}
\details{
The package provides a schema of S4 classes and methods to produce dataset
documentation. First, all classes defined in this package extend a virtual
``master class'' \linkS4class{cdbkObject}:
\itemize{
  \item Members of sub-class \linkS4class{cdbkDf} are designed to store
  data-level summaries like number of variables, number of cases, number of
  complete cases etc.

  \item Members of sub-class \linkS4class{cdbkVar} are designed to store
  variable-level summaries like length, number of missing values, mean, minimum
  and maximum, etc.

  \item Objects of sub-class \linkS4class{cdbkData} contain objects of the
  above two classes as slots and essentially collect all the neccessary
  information in one object.
}
These classes can be extended by the user to create objects that will contain
any information that one might need.

There is a collection of generic functions for these three basic classes that
actually compute the neccessary information from the data. For example the
generic \code{\link{cdbkTech}} creates simple ``technical`` summary of a
variable.

Finally there is a collection of methods for printing these summary objects.
Currently only printing to plain text is supported with a function
\code{\link{cdbkTxt}}.  There are plans for providing functions for writing
HTML and LaTeX code as well.
}
\author{
This package was written and is maintained by

Michal Bojanowski \email{michal2992@gmail.com}
}
\seealso{
Alternative set of functionality is also available in the \pkg{memisc} package.
}
\examples{
# example data frame
d <- data.frame( n = 1:5, # numeric variable
  ch = letters[1:5], # character variable
  f = factor(letters[6:10]), # factor
  i = as.integer(1:5) ) # integer variable

# make a summary of individual variable
o <- cdbkTech(d$n)
o
# print to txt
cdbkTxt(o)

# make a global sumary of the dataset
o <- cdbkDf(d)
o

# make a documentation object for the whole dataset
o <- cdbkData(d, "cdbkTech")
o
cdbkTxt(o)
}
