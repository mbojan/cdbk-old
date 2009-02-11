\name{cdbkData-class}
\docType{class}
\alias{cdbkData-class}
\alias{show,cdbkData-method}

\title{Data- and variable-level information}
\description{Objects of	this class collect all the information,
for both dataset and individual variables, that is necessary
for the production of the codebook.}
\section{Objects from the Class}{
Objects can be created by calls to the generic function
\code{\link{cdbkData}} which has it's own documentation page.
}
\section{Slots}{
	 \describe{
    \item{\code{name}:}{character, the name of the dataset}
    \item{\code{dat}:}{object of class \code{"cdbkDf"}, the dataset-level information}
    \item{\code{vars}:}{list of objects of class \code{"\link[=cdbkVar-class]{cdbkVar}"}}
  }
}
\section{Extends}{
Class \code{"\linkS4class{cdbkObject}"}, directly.
}
\section{Methods}{
  \describe{
    \item{cdbkTxt}{\code{signature(object = "cdbkData")}: ... }
    \item{show}{\code{signature(object = "cdbkData")}: ... }
	 }
}
\seealso{
Generic \code{\link{cdbkData}} for creating the objects of this class.
% TODO Add references to methods for this object.
}
\examples{
d <- data.frame(x=1:5, y=letters[1:5])
o <- cdbkData(d, "cdbkTech")
o
str(o)
}
