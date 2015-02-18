##' Repeat a call to FredR API function until all data is downloaded
##'
##' @param f one of the functions in the 'FredR' object that one wants to
##' call  repeatedly
##' @param ... additional parameters to FredR API function call
##' @param limit number of units to be returned in a single call to the API
##' @param offset offset
##' @return data.table with the resulting dataset
##' @author Janko Cizel
##' @export
repeatCall <- function(
    f = NULL,
    ...,
    limit = 1000,
    offset = 0
){
    o <- f(..., limit = limit, offset = offset)

    if (NROW(o) == 0)
        return(o)

    offset = offset + limit
    
    result <- rbindlist(
        list(
            o,
            repeatCall(f = f, ..., limit = limit, offset = offset)
        ),
        fill = TRUE,
        use.names = TRUE
    )

    return(result)
}


## ## World Penn Tables
## x <- repeatCall(f = fred$release.series, release_id = '285')
## x %>>%
## str


