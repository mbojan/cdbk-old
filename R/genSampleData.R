# example data set

genSampleData <- function( n=20, k=5, pna=.1 )
{
    # complete variables
    d1 <- data.frame( num = rnorm(n), # numeric variable
        ch = sample(letters[1:k], n, replace=TRUE), # character
        fac = factor( sample(letters[1:k], n, replace=TRUE) ), # factor
        int = as.integer( sample( 1:k, n, replace=TRUE) ), # integer
        stringsAsFactors=FALSE
        )
    # variables with some NAs
    d2 <- data.frame( num = rnorm(n), # numeric variable
        ch = sample(letters[1:k], n, replace=TRUE), # character
        fac = factor( sample(letters[1:k], n, replace=TRUE) ), # factor
        int = as.integer( sample( 1:k, n, replace=TRUE) ), # integer
        stringsAsFactors=FALSE
        )
    names(d2) <- paste( names(d2), "na", sep="")
    d2 <- as.data.frame( lapply(d2, function(x)
        {
            x[ sample(1:nrow(d2), round(pna*nrow(d2)), replace=FALSE) ] <- NA
            x
        } ), stringsAsFactors=FALSE )
    cbind(d1, d2)
}
