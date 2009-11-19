\name{cdbkOpts}
\alias{cdbkOpts}
\alias{.cdbk.options}
\title{Options of package 'cdbk'}
\description{
Interface to package \pkg{cdbk} global options.
}
\usage{
cdbkOpts(..., ns = "cdbk", oname = ".cdbk.options")
.cdbk.options # not exported
}
\arguments{
  \item{\dots}{Either a character vector giving the names of the options
  to read or a collection of expressions \code{tag=value}, see Details}
  \item{ns}{character, name of the namespace where the assignment should
  take place. Defaults to the namespace of package \pkg{cdbk}}
  \item{oname}{character, name of the object storing options}
}
\details{
Package options are stored in the unexported list called \code{.cdbk.options}.
This function provides an interface, not unlike \code{\link{par}} or
\code{\link{options}}, for this list.

The option values can be accessed using calls like
\code{cdbkOpts("optionname")}, where \code{"optionname"} is a name of the
option to get.

Options can be set using calls like \code{cdbkOpts(optname=optvalue)} where
\code{optname} is the name of the option and \code{optvalue} is the new value
for that option.
}
\value{
If called without any arguments the function returns the list with all options.

If called with character vector as a single argument a list of options with
names provided in the argument.

If called with a collection of tag=value statements a list with current option
settings (before the new ones are applied).

If the function is called with some non-existent option names it throws a
warning in case of reading and an error if setting.
}
\section{Options}{
% list of options in .cdbk.options
\describe{
  \item{defvar}{character vector with names of the functions used to produce
  default reports for a variable.}
  \item{defdf}{character vector of with names of the functions used to produce
  default reports for a data frame.}
}
}
\examples{
# All options
cdbkOpts()

# get values of options 'defvar' and 'defdf'
cdbkOpts( c("defvar", "defdf") )

\dontrun{
# set values of options 'defvar' and 'defdf',
# 'op' is a list of all the options before 'foo' and 'bar' got new values
op <- cdbkOpts(defvar=1, defdf=2)
# TODO to be implemented: calling 'cdbkOpts' with a list
cdbkOpts(op)
}
}
