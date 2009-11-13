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

# class for data frame descriptions
setClass("cdbkDf", contains="cdbkObject")

# class for allowable types of variable vectors
setClassUnion( "cdbkVector", c("numeric", "character", "integer", "logical", "factor"))

# for size slots with values from calling object.size()
setOldClass("object_size")


#-------------------------------------------------------------------------------
# Classes for variables
#-------------------------------------------------------------------------------

# Brief, technical, summary of a (vector) variable.

setClass("varTech", representation(
	varname="character", # variable name
	varclass="character", # variable class
	varmode="character", # variable storage mode
	hasnames="logical", # is there a names attribute
	uvalues="numeric", # number of unique values
	nas="numeric", # number of NAs
	size="object_size"), # size in bytes
    contains="cdbkVar"
)




# Frequency table

setClass("varFreq", representation(
	varname="character",
	freq = "numeric",
	pct="numeric",
	labels="character"),
    contains="cdbkVar")

setValidity("varFreq",
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



#-------------------------------------------------------------------------------
# Classes for data frames
#
# Data-level descriptions
#-------------------------------------------------------------------------------


# Class for data frames. Contains some data-level descriptives: number of
# variables, number of observations, number of complete cases.

setClass("dfTech", representation(
	ncases="numeric", # number of rows
	nvars="numeric", # number of variables
	ncomplete="numeric", # number of complete cases
	size="object_size"), # size in bytes
    contains="cdbkDf")




#===============================================================================
###| Generics
#===============================================================================

# constructors for classes
setGeneric("varTech", function(object, ...) standardGeneric("varTech"))
setGeneric("varFreq", function(object, ...) standardGeneric("varFreq"))
setGeneric("dfTech", function(object, ...) standardGeneric("dfTech"))

setGeneric("cdbkTxt", function(object, ...) standardGeneric("cdbkTxt"))


# misc utilities
setGeneric("prettyMatrix", function(object) standardGeneric("prettyMatrix"))








#===============================================================================
###| Methods
#===============================================================================




#-------------------------------------------------------------------------------
# Generic functions for constructing the objects
#-------------------------------------------------------------------------------


setMethod("varTech", "cdbkVector",
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
    new("varTech", varname=vname,
        varclass=varclass,
        varmode=varmode,
        hasnames=hasnames,
        nas=nas,
        uvalues=uvalues,
        size=size )
} )




setMethod("varFreq", "cdbkVector",
function(object, varname=NULL, ...)
{
    if(is.null(varname))
        vname <- deparse(substitute(object, parent.frame()))
    else vname <- varname
        tab <- table(object, exclude=NULL)
    new("varFreq", varname=vname, freq=as.numeric(tab),
	pct=as.numeric(tab/sum(tab)*100),
	labels=paste(names(tab)) )
} )
    
    












#===============================================================================
# Constructing 'dfTech' objects
#===============================================================================




setMethod("dfTech", "data.frame",
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
# 'show' methods
#===============================================================================


setMethod("show", "varTech",
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





setMethod("show", "dfTech",
function(object)
{
    cat("Data frame summary\n")
    cat("  Number of cases:                ", object@ncases, "\n")
    cat("  Number of variables:            ", object@nvars, "\n")
    cat("  Number of complete cases:       ", object@ncomplete, "\n")
    cat("  Size:                           ", object@size, "\n")
    cat("\n")
} )








#===============================================================================
# Helper functions for pretty printing
#===============================================================================

# Layout the object information in a matrix for pretty-printing


setMethod("prettyMatrix", "varTech",
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




setMethod("cdbkTxt", "varTech",
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






setMethod("cdbkTxt", "varFreq",
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
    
    

