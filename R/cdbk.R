#===============================================================================
# Documenting datasets
#===============================================================================

# Package structure
# -----------------
#
# Set of classes for data objects which store computed summaries. Data objects
# include variables (vectors) as well as data frames.
#
# Set of generic functions for printing the objects to specific output styles:
# text, latex, html...
#
#
# Desired functionality
# ---------------------
#
# Generate data dependent Sweave file that will generate a code book in LaTeX













#===============================================================================
###| Classes
#===============================================================================



# Class for all objects
setClass("cdbkObject")

# class for variable descriptions
setClass("cdbkVar", contains="cdbkObject")


# class for allowable types of variable vectors
setClassUnion( "cdbkVector", c("numeric", "character", "integer", "logical", "factor"))


#-------------------------------------------------------------------------------
# Classes for variables
#-------------------------------------------------------------------------------

# Brief, technical, summary of a (vector) variable.

setClass("cdbkTech", representation(
	varname="character", # variable name
	varclass="character", # variable class
	varmode="character", # variable storage mode
	hasnames="logical", # is there a names attribute
	uvalues="numeric", # number of unique values
	nas="numeric", # number of NAs
	size="numeric"), # size in bytes
    contains="cdbkVar"
)


#-------------------------------------------------------------------------------
# Classes for data frames
#-------------------------------------------------------------------------------


# Class for data frames. Contains some data-level descriptives: number of
# variables, number of observations, number of complete cases.

setClass("cdbkDf", representation(
	ncases="numeric", # number of rows
	nvars="numeric", # number of variables
	ncomplete="numeric", # number of complete cases
	size="numeric"), # size in bytes
    contains="cdbkObject")

#-------------------------------------------------------------------------------
# Combined information on variables and data frame
#-------------------------------------------------------------------------------


# Class for "complete" data set

setClass("cdbkData", representation(
	name="character", # name of the data set
	dat="cdbkDf", # data frame information
	vars="list"), # list of variable documentation objects
    contains="cdbkObject")


setValidity("cdbkData",
function(object)
{
    rval <- NULL
    # check if all elements of 'vars' contain 'cdbkVar' objects
    isvar <- sapply( object@vars,
	function(x) extends( data.class(x), "cdbkVar") )
    if( !all(isvar) )
	rval <- c(rval, paste("not all components of 'vars' extend class 'cdbkVar':",
	    paste(which(!isvar), collapse=", ") ))
    if(is.null(rval))
	return(TRUE)
    else return(rval)
} )



#-------------------------------------------------------------------------------

# frequency tables

setClass("cdbkFreq", representation(
	varname="character",
	freq = "numeric",
	pct="numeric",
	labels="character"),
    contains="cdbkVar")

setValidity("cdbkFreq",
function(object)
{
    rval <- NULL
    if(length(object@freq) != length(object@pct))
	rval <- c(rval, "length mismatch for freq and pct")
    if(length(object@freq) != length(object@labels))
	rval <- c(rval, "length mismatch for freq and labels")
    if(is.null(rval))
	return(TRUE)
    else return(rval)
} )





#===============================================================================
###| Generics
#===============================================================================

# constructors for classes
setGeneric("cdbkTech", function(object, ...) standardGeneric("cdbkTech"))
setGeneric("cdbkDf", function(object, ...) standardGeneric("cdbkDf"))
setGeneric("cdbkData", function(dat, vars, ...) standardGeneric("cdbkData"))
setGeneric("cdbkFreq", function(object, ...) standardGeneric("cdbkFreq"))

# printing methods to various formats
setGeneric("cdbkTxt", function(object, ...) standardGeneric("cdbkTxt"))

# misc utilities
setGeneric("prettyMatrix", function(object) standardGeneric("prettyMatrix"))








#===============================================================================
###| Methods
#===============================================================================




#-------------------------------------------------------------------------------
# Generic functions for constructing the objects
#-------------------------------------------------------------------------------


setMethod("cdbkTech", "cdbkVector",
function(object, varname=NULL, 
    varclass=class(object),
    varmode=storage.mode(object),
    hasnames=!is.null(names(object)),
    uvalues=length(unique(object)),
    nas=sum(is.na(object)),
    size=object.size(object), ... )
{
    if(is.null(varname))
	vname <- deparse(substitute(object, parent.frame()))
    else vname <- varname
    new("cdbkTech", varname=vname,
	varclass=varclass,
	varmode=varmode,
	hasnames=hasnames,
	nas=nas,
	uvalues=uvalues,
	size=size )
} )




setMethod("cdbkFreq", "cdbkVector",
function(object, varname=NULL, ...)
{
    if(is.null(varname))
	vname <- deparse(substitute(object, parent.frame()))
    else vname <- varname
    tab <- table(object, exclude=NULL)
    new("cdbkFreq", varname=vname, freq=as.numeric(tab),
	pct=as.numeric(tab/sum(tab)*100),
	labels=paste(names(tab)) )
} )
    
    












#===============================================================================
# Constructing 'cdbkDf' objects
#===============================================================================




setMethod("cdbkDf", "data.frame",
function(object,
    ncases=nrow(object), nvars=ncol(object),
    size=object.size(object), ...)
{
    # calculate the number of complete observations and create the object
    new("cdbkDf", ncases=ncases, nvars=nvars,
	ncomplete=sum( completeCases(object) ),
	size=size )
} )


    



#===============================================================================
# Constructing 'cdbkData' objects
#===============================================================================

setMethod("cdbkData", signature( dat="cdbkDf", vars="list"),
function( dat, vars, name=deparse(substitute(dat)), ...)
{
    new("cdbkData", dat=dat, vars=vars, name=name)
} )

# If 'varlist' is a character then it specifies the name of the variable
# processing function. The variable processing functions is assumed to take
# at least two arguments (in that order) 'dat' variable vector, and 'varname'
# for a variable name as character.
setMethod("cdbkData", signature( dat="data.frame", vars="character"),
function( dat, vars, name=deparse(substitute(dat)), ... )
{
    d <- cdbkDf( dat )
    # make variable doc objects
    vn <- names(dat)
    l <- lapply( vn, function(x) do.call(vars, list(object=dat[[x]], varname=x)))
    cdbkData( dat=d, vars=l, name=name, ...)
} )
    



#===============================================================================
# 'show' methods
#===============================================================================


setMethod("show", "cdbkTech",
function(object)
{
    cat("Variable summary\n")
    cat("  Name:                    ", object@varname, "\n")
    cat("  Class:                   ", paste(object@varclass, collapse=", "), "\n")
    cat("  Storage mode:            ", object@varmode, "\n")
    cat("  Number of unique values: ", object@uvalues, "\n")
    cat("  Number of NAs:           ", object@nas, "\n")
    cat("  Has names:               ", object@hasnames, "\n")
    cat("  Size [bytes]:            ", object@size, "\n")
    cat("\n")
} )





setMethod("show", "cdbkDf",
function(object)
{
    cat("Data frame summary\n")
    cat("  Number of cases:                ", object@ncases, "\n")
    cat("  Number of variables:            ", object@nvars, "\n")
    cat("  Number of complete cases:       ", object@ncomplete, "\n")
    cat("  Size:                           ", object@size, "\n")
    cat("\n")
} )





setMethod("show", "cdbkData",
function(object)
{
    show( object@dat )
    cat("  Number of documented variables:", length(object@vars), "\n")
} )




#===============================================================================
# Helper functions for pretty printing
#===============================================================================

# Layout the object information in a matrix for pretty-printing


setMethod("prettyMatrix", "cdbkTech",
function(object)
{
    m <- matrix("", 3, 4)
    m[,1] <- c("Class:",
               "Storage mode:", 
	       "Size:")
    m[,2] <- as.character(c( paste(object@varclass, collapse=", "), object@varmode, object@size))
    m[,3] <- c("Unique values:",
               "Number of NAs:",
	       "Has names:")
    m[,4] <- as.character(c(object@uvalues, object@nas, as.character(object@hasnames)))
    dimnames(m) <- list( rep("", nrow(m)), rep("", ncol(m)))
    m
} )







if(FALSE)
{
    prettyMatrix(cdbkTech(of))
}


#===============================================================================
# pretty printing to text format
#===============================================================================




setMethod("cdbkTxt", "cdbkTech",
function(object, file="", append=TRUE, ...)
{
    m <- prettyMatrix(object)
    wd <- getOption("cdbkHeaderWidth")
    # begin writing with a header
    cat( paste( paste(rep("=", wd), collapse=""), "\n", sep=""),
	append=append, file=file)
    cat( "[", object@varname, "]", "\n", sep="", append=TRUE, file=file)
    cat( paste( paste(rep("=", wd), collapse=""), "\n", sep=""),
	append=TRUE, file=file)
    out <- apply(m, 1, txtPrettyRow)
    dimnames(out) <- list( rep("", nrow(out)), rep("", ncol(out)))
    if(file=="")
    {
	print( t(out), quote=FALSE, print.gap=4)
	cat("\n\n")
    }
    else {
	sink(file=file, append=TRUE)
	print( t(out), quote=FALSE, print.gap=4)
	cat("\n\n\n")
	sink()
    }
} )


if(FALSE)
{
    o <- cdbkTech(of)
    m <- prettyMatrix(o)
    cdbkTxt(o)
    cdbkTxt(o, file="dupa.txt")
}






setMethod("cdbkTxt", "cdbkFreq",
function(object, file="", append=FALSE, ...)
{
    d <- data.frame(n=object@freq, pct=object@pct, row.names=object@labels)
    if(file=="")
	print(d, quote=FALSE)
    else
    {
	sink(file=file, append=TRUE)
	print( d, quote=FALSE, print.gap=4)
	cat("\n\n\n")
	sink()
    }
} )


#===============================================================================
# methods for data frames
#===============================================================================



setMethod("cdbkTxt", "list",
function(object, ...)
{
    # TODO Add a check, whether components are of proper class
    invisible(lapply(object, cdbkTxt, ...))
} )


setMethod("cdbkTxt", "cdbkData",
function(object, file="", append=FALSE,
    title="Dataset documentation")
# file = name of the connection where to write results, if "" then to console
# append = if file!="" then whether to append the file
# title = informative title at the top of the printout
{
    wd <- getOption("cdbkHeaderWidth")
    titlen <- nchar(title)
    # centered title
    titline <- paste( paste(rep(" ", (wd-titlen)%/%2), collapse=""),
	title,
	paste(rep(" ", (wd-titlen)%/%2), collapse=""),
	sep="" )
    cat(titline, "\n", file=file, append=append, sep="")
    cat("\n\n\n", file=file, append=TRUE)
    cat("Dataset name: ", object@name, "\n", file=file, append=TRUE)
    cat("Number of cases: ", object@dat@ncases, "\n", file=file, append=TRUE)
    cat("Number of variables: ", object@dat@nvars, "\n", file=file, append=TRUE)
    cat("Number of complete cases: ", object@dat@ncomplete, "\n", file=file, append=TRUE)
    cat("Size [bytes]: ", object@dat@size, "\n", file=file, append=TRUE)
    cat("Number of documented variables: ", length(object@vars), "\n", file=file, append=TRUE)
    cat("\n\n", file=file, append=TRUE)
    cdbkTxt( object@vars, file=file, append=TRUE)
} )



    


if(FALSE)
{
    d <- data.frame( n=rnorm(10), f=factor(1:10), ch=letters[1:5])
    o <- cdbkTech(d)
    cdbkTxt(o)
    cdbkTxt(o, file="dupa2.txt")
}






#===============================================================================
###| Misc functions
#===============================================================================

    


# Helper for calculating the number of complete (non-NA) cases in a data frame.
# Returns a logical vector whether a case has any NAs
completeCases <- function(d)
{
    rval <- rep(TRUE, nrow(d))
    for( vind in seq(1, ncol(d)) )
    {
	w <- which(is.na( d[[vind]] ) )
	rval[w] <- FALSE
    }
    rval
}






# Given a row of the pretty matrix collapse labels with values

txtPrettyRow <- function(r, sep=" ")
{
    if( (length(r) %% 2) != 0 )
	stop("even number of elements required")
    # position of the labels
    labid <- seq(1, length(r)-1, by=2)
    conid <- seq(2, length(r), by=2)
    rval <- paste( r[labid], r[conid], sep=" ")
    rval
}
    
    

