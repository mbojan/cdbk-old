
.onLoad <- function(libname, pkgname)
{
    # set package options
    options(
	cdbkHeaderWidth = getOption("width") ) # width for text mode headers
}
    
