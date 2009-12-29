sumvar <- function(object, ...)
{
    # NAs
    tab <- t(sapply(object, function(x) table(factor(is.na(x),
        levels=c(FALSE, TRUE)))) )
    colnames(tab) <- c("nonNAs", "NAs")
    # uniques
    uni <- sapply(object, function(x) length(unique(x)) )
    # mode
    mode <- sapply(object, mode)
    rval <- cbind( mode=mode, uniques=uni, tab)
    class(rval) <- c("sumvar", "matrix")
    rval
}

print.sumvar <- function(x, ...)
{
    cat("Summary of variables\n")
    class(x) <- "matrix"
    print.default(x, quote=FALSE)
}
