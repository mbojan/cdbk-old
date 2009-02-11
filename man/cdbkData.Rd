\name{cdbkData}
\alias{cdbkData}
\docType{methods}
\alias{cdbkData-methods}
\alias{cdbkData,cdbkDf,list-method}
\alias{cdbkData,data.frame,character-method}
\title{Constructing data documentation object}
\description{
This function constructs a data documentation object that stores
all the relevant information of the whole dataset that will be used
to producde the codebook. The object contains dataset-level information
like e.g. name as well as the list of variable-level information objects.
}
\usage{
cdbkData(dat, vars, ...)
\S4method{cdbkData}{cdbkDf,list}(dat, vars, name=deparse(substitute(dat)), ...)
\S4method{cdbkData}{data.frame,character}(dat, vars,
  name=deparse(substitute(dat)), ...)
}
\arguments{
  \item{dat}{object, the data set, see Methods and Details}
  \item{vars}{object, variable-level information, see Methods and Details}
  \item{name}{character, name of the dataset}
  \item{\dots}{other arguments passed from/to other methods}
}
\details{
The objects of class \code{cdbkData} collect all the information neccessary for
producing the codebook.

The information on the level of the dataset is supplied through the \code{dat}
argument. Currently methods are defined for objects of class
\code{"\link[=cdbkDf-class]{cdbkDf}"} or \code{data.frame}s.

Variable-level information can be supplied for the \code{vars} argument can be
supplied in two forms. Either as a list of
\code{"\link[=cdbkVar-class]{cdbkVar}"} objects or as a character name of the
variable-processing function. This function is assumed to take at least
two arguments: \code{dat} for the data vector, and \code{varname} for
the variable name. See \code{\link{cdbkTech}} for an example.

See below for a list of currently implemented methods.
}
\section{Methods}{
\describe{
Currently implemented methods are:

\item{dat = "cdbkDf", vars = "list"}{The method simply collects the data-level
information from the \code{dat} argument, and list of variable-level
information in the form of objects of class
\code{"\link[=cdbkVar-class]{cdbkVar}"}.  These two are put into the
corresponding slots of the resulting object.}

\item{dat = "data.frame", vars = "character"}{The data frame supplied in
\code{dat} argument is converted to \code{"\link[=cdbkDf-class]{cdbkDf}"}
class. The \code{vars} argument is the name of the function the produces
variable-level documentation object. It is applied to all variables in
\code{dat}. The two resulting objects are then collected together using the
previously described method.}
}}
\value{
Object of class \code{"\link[=cdbkData-class]{cdbkData}"}.
}
\seealso{Description of classes \code{"\link[=cdbkData-class]{cdbkData}"},
\code{"\link[=cdbkDf-class]{cdbkDf}"}, and \code{"\link[=cdbkVar-class]{cdbkVar}"}.

\code{cdbkTech} for an example of variable-summarizing function.
}
\examples{
d <- data.frame(n=1:5, ch=letters[1:5], f=factor(letters[1:5]),
  stringsAsFactors=FALSE)
cdbkData(d, "cdbkTech")
}
