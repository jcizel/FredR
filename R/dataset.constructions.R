## debt.securities.statistics <- function(fred = NULL){
##     if (is.null(fred) | !inherits(fred,'FredR'))
##         stop('`fred` is a required argument and must be of class `FredR`')
    
##     static <- repeatCall(f = fred$release.series, release_id = '327')

##     ## Select only series with known issuers
##     static$title %>>%
##     (grepl(
##         pattern = "([.]*)(RESIDENCE OF ISSUER IN|NATIONALITY OF ISSUER IN)([.]*)",
##         x = .,
##         ignore.case = TRUE
##     )) -> sel

##     ## Create lookup table
##     static[sel] %>>%
##     copy %>>%
##     (.[, label := {title %>>%
##                    toupper %>>%
##                    (stri_replace_all_regex(
##                        str = .,
##                        pattern = "(.+)(RESIDENCE OF ISSUER IN .+|NATIONALITY OF ISSUER IN .+)",
##                        replacement = "$1"
##                    )) %>>%
##                    str_trim %>>%
##                    (stri_replace_all_regex(
##                        str = .,
##                        pattern = ",$",
##                        replacement = ""
##                    )) %>>%                               
##                    iconv(
##                        to = 'ASCII',
##                        sub = ''
##                    ) %>>%
##                    stri_trans_totitle
##                }])


##     ## pull time series
##     static[sel]$id %>>%
##     list.map(
##         fred$series.observations(series_id = .)
##     ) %>>%
##     rbindlist(fill = TRUE, idcol = TRUE) ->
##         dt

##     ## Reshape dataset
##     dt %>>%
##     subset(
##         .id %in% unique(static[sel]$id) 
##     ) %>>%
##     mutate(
##         code = .id %>>% str_sub(end = -3),
##         iso2 = .id %>>% str_sub(start = -2),
##         value = value %>>% as.numeric
##     ) %>>%
##     dcast.data.table(
##         iso2 + date ~ code,
##         value.var = 'value'
##     ) %>>%
##     mutate(
##         date = as.Date(date)
##     ) ->
##         dt.r    

##     out <- structure(dt.r,
##                      lookup = lookuptable)

##     save(out, file = './inst/data/debt_securities_statistics.RData')
    
##     return(out)
## }


## govt.bond.yields.oecd <- function(fred){
##     if (is.null(fred) | !inherits(fred,'FredR'))
##         stop('`fred` is a required argument and must be of class `FredR`')
    
##     static <- fred$tags.series(tag_names =
##                                    c('bonds',
##                                      'government',
##                                      'nation',
##                                      'yield',
##                                      'monthly',
##                                      '10-year'))

##     static$id %>>%
##     list.map(
##         fred$series.observations(series_id = .)
##     ) %>>%
##     rbindlist(fill = TRUE, idcol = TRUE)  ->
##         dt

##     dt %>>%
##     mutate(
##         code = substr(.id, start = 1, stop = 8),
##         iso2 = substr(.id, start = 9, stop = 10),
##         value = value %>>% as.numeric
##     ) %>>% 
##     dcast.data.table(
##         iso2 + date ~ code,
##         value.var = 'value'
##     ) %>>%
##     subset(
##         complete.cases(.)
##     ) %>>%    
##     mutate(
##         date = as.Date(date)
##     ) ->
##         dt.r

##     out <- structure(dt.r,
##                      lookup = static)

##     save(out, file = './inst/data/govt_bond_yields_oecd.RData')
    
##     return(out)    
## }


## dataset.constructors <- list(
##     "Debt Securities Statistics" = debt.securities.statistics,
##     "10-Y Government Bond Yields (monthly, OECD)" = govt.bond.yields.oecd
## )
