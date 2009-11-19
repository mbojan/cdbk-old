cdbkOpts <-
function(..., ns="cdbk", oname=".cdbk.options")
{
    # if empty, return the list of options
    if(nargs() == 0 ) return(getFromNamespace(oname, ns=ns))
    l <- list(...)
    current <- getFromNamespace(oname, ns)
    # if without names => getting the values
    if( is.null(names(l)) )
    {
        nams <- l[[1]]
        # check if all present and warning if unknown
        ind <- nams %in% names(current)
        if( !all(ind) )
            warning("unknown options:", nams[!ind])
        # return current values
        current[ind]
    } else
    {
        # with names the set new values and return current
        nams <- names(l)
        # check if all present and stop if unknown
        ind <- nams %in% names(current)
        if( !all(ind) )
            stop("unknown options:", nams[!ind])
        # assign new values
        temp <- current
        temp[nams] <- l
        assignInNamespace(oname, temp, ns=ns)
        invisible(current)
    }
}


# Default option list
.cdbk.options <- list(
    defvar = c("varTech", "varFreq"),
    defdf = "dfTech"
    )
