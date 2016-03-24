##' FredR: R Interface to Federal Reserve Economic Data API
##'
##' This package provides an interface to the Federal Reserve Economic Data
##' (FRED) API. FRED covers 240,000 US and international macroeconomic time
##' series from 77 sources (including the World Bank,  OECD, and BIS).
##' FredR has been designed in a way that gives a user a complete interface to
##' all available functionalities of the API.
##'
##' 
##' @title FredR: R Interface to Federal Reserve Economic Data API
##' @param api.key API key (required). You can get the key by registering at the
##' following address: http://api.stlouisfed.org/api_key.html
##
##' @return List object of class FredR. The list object consists of functions
##' that perform the API calls (to be documented soon). For now, one can find
##' the descriptions of particular calls and their parameters here: http://api.stlouisfed.org/docs/fred/?utm_source=research&utm_medium=website&utm_campaign=data-tools
##' @author Janko Cizel
##' @export
##' @import XML RCurl data.table pipeR rlist dplyr
FredR <- function(api.key = NULL){

    if (is.null(api.key))
        stop("Provide an API key! You can obtain one here: http://api.stlouisfed.org/api_key.html")

    root = 'https://api.stlouisfed.org'

    xml2dt <- function(xmlObj){
        xmlApply(xmlRoot(xmlObj), xmlAttrs) %>>%
        list.map(. %>>% as.list %>>% as.data.table) %>>%
        rbindlist(fill = TRUE) ->
            dt        
        return(dt)
    }
    
    ## recursive function: iteratively look for children
    getCategory <- function(category.id){
        query = 'fred/category'
        getURL(
            url = sprintf(
                "%s/%s?category_id=%s&api_key=%s",
                root,
                query,
                category.id,
                api.key
            ) %>>%
            URLencode
        ) %>>%
        xmlTreeParse(
            useInternalNodes = TRUE
        ) %>>%
        xmlRoot %>>%
        xmlApply(xmlAttrs)
    }
    ## getCategory(0)
    
    getChildrenList <- function(category.id){
        query = 'fred/category/children'
        getURL(
            url = sprintf(
                "%s/%s?category_id=%s&api_key=%s",
                root,
                query,
                category.id,
                api.key
            ) %>>%
            URLencode
        ) %>>%
        xmlTreeParse(
            useInternalNodes = TRUE
        ) %>>%
        xmlRoot %>>%
        xmlApply(xmlAttrs)
    }
    
    traverseCategories = function(category.id = 0){
        cat(category.id,"\n")
        
        l <- getChildrenList(category.id)
        if (names(l) == 'text' & is.null(l[['text']])){
            getCategory(category.id) %>>%
            list.map(
                as.list(.)
            ) %>>% (category) -> final
            return(
                list(
                    id = final[['id']],
                    parent.id = final[['parent_id']],
                    name = final[['name']]
                )
            )
        }
        
        l %>>%
        list.map({
            temp <- . %>>% as.list
            list(
                id = temp[['id']],
                parent.id = category.id,
                name = temp[['name']],
                children = traverseCategories(temp[['id']])
            )
        }) ->
            o

        ## Final delivery
        names(o) <- NULL

        return(o)
    }



    ## ---------------------------------------------------------------------- ##
    ## CATEGORIES                                                             ##
    ## ---------------------------------------------------------------------- ##
    category <- function(
        category_id = NULL
    ){
        if (is.null(category_id))
            stop("category_id is a required argument")

        query = 'fred/category'
        url = sprintf(
            "%s/%s?&api_key=%s",
            root,
            query,
            api.key
        )

        if (!is.null(category_id))
            sprintf('%s&category_id=%s',url,category_id) -> url        
        
        getURL(
            url = url %>>%
            URLencode
        ) %>>%
        xmlTreeParse(
            useInternalNodes = TRUE
        ) -> l

        dt <- xml2dt(l) 
        
        return(dt)
    }

    category.children <- function(
        category_id = NULL
    ){
        if (is.null(category_id))
            stop("category_id is a required argument")

        query = 'fred/category/children'
        url = sprintf(
            "%s/%s?&api_key=%s",
            root,
            query,
            api.key
        )

        if (!is.null(category_id))
            sprintf('%s&category_id=%s',url,category_id) -> url        
        
        getURL(
            url = url %>>%
            URLencode
        ) %>>%
        xmlTreeParse(
            useInternalNodes = TRUE
        ) -> l

        dt <- xml2dt(l) 
        
        return(dt)
    }    

    category.related <- function(
        category_id = NULL
    ){
        if (is.null(category_id))
            stop("category_id is a required argument")

        query = 'fred/category/related'
        url = sprintf(
            "%s/%s?&api_key=%s",
            root,
            query,
            api.key
        )

        if (!is.null(category_id))
            sprintf('%s&category_id=%s',url,category_id) -> url        
        
        getURL(
            url = url %>>%
            URLencode
        ) %>>%
        xmlTreeParse(
            useInternalNodes = TRUE
        ) -> l

        dt <- xml2dt(l) 
        
        return(dt)
    }    


    category.series <- function(
        category_id = NULL,
        limit = 1000,
        offset = NULL
    ){
        if (is.null(category_id))
            stop("category_id is a required argument")
        if (limit > 1000 | limit < 0)
            stop("Limit must be between 0 and 1000.")        

        query = 'fred/category/series'
        url = sprintf(
            "%s/%s?&api_key=%s",
            root,
            query,
            api.key
        )

        if (!is.null(category_id))
            sprintf('%s&category_id=%s',url,category_id) -> url
        if (!is.null(limit))
            sprintf('%s&limit=%s',url,limit) -> url
        if (!is.null(offset))
            sprintf('%s&offset=%s',url,offset) -> url                        
        
        getURL(
            url = url %>>%
            URLencode
        ) %>>%
        xmlTreeParse(
            useInternalNodes = TRUE
        ) -> l

        dt <- xml2dt(l) 
        
        return(dt)
    }

    category.tags <- function(
        category_id = NULL,
        limit = 1000
    ){
        if (is.null(category_id))
            stop("category_id is a required argument")
        if (limit > 1000 | limit < 0)
            stop("Limit must be between 0 and 1000.")        

        query = 'fred/category/tags'
        url = sprintf(
            "%s/%s?&api_key=%s",
            root,
            query,
            api.key
        )

        if (!is.null(category_id))
            sprintf('%s&category_id=%s',url,category_id) -> url
        if (!is.null(limit))
            sprintf('%s&limit=%s',url,limit) -> url                
        
        getURL(
            url = url %>>%
            URLencode
        ) %>>%
        xmlTreeParse(
            useInternalNodes = TRUE
        ) -> l

        dt <- xml2dt(l) 
        
        return(dt)
    }

    category.related_tags <- function(
        category_id = NULL,
        limit = 1000
    ){
        if (is.null(category_id))
            stop("category_id is a required argument")
        if (limit > 1000 | limit < 0)
            stop("Limit must be between 0 and 1000.")        

        query = 'fred/category/related_tags'
        url = sprintf(
            "%s/%s?&api_key=%s",
            root,
            query,
            api.key
        )

        if (!is.null(category_id))
            sprintf('%s&category_id=%s',url,category_id) -> url
        if (!is.null(limit))
            sprintf('%s&limit=%s',url,limit) -> url                
        
        getURL(
            url = url %>>%
            URLencode
        ) %>>%
        xmlTreeParse(
            useInternalNodes = TRUE
        ) -> l

        dt <- xml2dt(l) 
        
        return(dt)
    }    

    ## ---------------------------------------------------------------------- ##
    ##                             RELEASES                                   ##
    ## ---------------------------------------------------------------------- ##
    releases <- function(
        limit = 1000
    ){
        if (limit > 1000 | limit < 0)
            stop("Limit must be between 0 and 1000.")
        
        query = 'fred/releases'
        url = sprintf(
            "%s/%s?&api_key=%s",
            root,
            query,
            api.key
        )

        if (!is.null(limit))
            sprintf('%s&limit=%s',url,limit) -> url        
        
        getURL(
            url = url %>>%
            URLencode
        ) %>>%
        xmlTreeParse(
            useInternalNodes = TRUE
        ) -> l

        dt <- xml2dt(l) 

        return(dt)                
    }

    releases.dates <- function(
        limit = 1000,
        include_release_dates_with_no_data = 'false'
    ){
        if (limit > 1000 | limit < 0)
            stop("Limit must be between 0 and 1000.")
        
        query = 'fred/releases/dates'
        url = sprintf(
            "%s/%s?&api_key=%s",
            root,
            query,
            api.key
        )

        if (!is.null(limit))
            sprintf('%s&limit=%s',url,limit) -> url
        if (!is.null(include_release_dates_with_no_data))
            sprintf('%s&include_release_dates_with_no_data=%s',url,include_release_dates_with_no_data) -> url        
        
        getURL(
            url = url %>>%
            URLencode
        ) %>>%
        xmlTreeParse(
            useInternalNodes = TRUE
        ) -> l

        dt <- xml2dt(l) 

        return(dt)                
    }


    release <- function(
        release_id = NULL,
        limit = 1000
    ){
        if (is.null(release_id))
            stop('release_id is a required argument.')
        if (limit > 1000 | limit < 0)
            stop("Limit must be between 0 and 1000.")
        
        query = 'fred/release'
        url = sprintf(
            "%s/%s?&api_key=%s",
            root,
            query,
            api.key
        )

        if (!is.null(release_id))
            sprintf('%s&release_id=%s',url,release_id %>>% paste(collapse=";")) -> url        
        if (!is.null(limit))
            sprintf('%s&limit=%s',url,limit) -> url
        
        getURL(
            url = url %>>%
            URLencode
        ) %>>%
        xmlTreeParse(
            useInternalNodes = TRUE
        ) -> l

        dt <- xml2dt(l) 

        return(dt)                
    }

    release.dates <- function(
        release_id = NULL,
        limit = 1000
    ){
        if (is.null(release_id))
            stop('release_id is a required argument.')
        if (limit > 1000 | limit < 0)
            stop("Limit must be between 0 and 1000.")
        
        query = 'fred/release/dates'
        url = sprintf(
            "%s/%s?&api_key=%s",
            root,
            query,
            api.key
        )

        if (!is.null(release_id))
            sprintf('%s&release_id=%s',url,release_id %>>% paste(collapse=";")) -> url        
        if (!is.null(limit))
            sprintf('%s&limit=%s',url,limit) -> url
        
        getURL(
            url = url %>>%
            URLencode
        ) %>>%
        xmlTreeParse(
            useInternalNodes = TRUE
        ) -> l

        dt <- xml2dt(l) 

        return(dt)                
    }

    release.series <- function(
        release_id = NULL,
        limit = 1000,
        offset = NULL
    ){
        if (is.null(release_id))
            stop('release_id is a required argument.')
        if (limit > 1000 | limit < 0)
            stop("Limit must be between 0 and 1000.")
        
        query = 'fred/release/series'
        url = sprintf(
            "%s/%s?&api_key=%s",
            root,
            query,
            api.key
        )

        if (!is.null(release_id))
            sprintf('%s&release_id=%s',url,release_id %>>% paste(collapse=";")) -> url        
        if (!is.null(limit))
            sprintf('%s&limit=%s',url,limit) -> url
        if (!is.null(offset))
            sprintf('%s&offset=%s',url,offset) -> url                                
        
        getURL(
            url = url %>>%
            URLencode
        ) %>>%
        xmlTreeParse(
            useInternalNodes = TRUE
        ) -> l

        dt <- xml2dt(l) 

        return(dt)                
    }

    release.tags <- function(
        release_id = NULL,
        limit = 1000,
        tag_names = NULL,
        exclude_tag_names = NULL,
        tag_group_id = NULL,
        search_text = NULL
    ){
        if (is.null(release_id))
            stop('release_id is a required argument.')
        if (limit > 1000 | limit < 0)
            stop("Limit must be between 0 and 1000.")
        
        query = 'fred/release/tags'
        url = sprintf(
            "%s/%s?&api_key=%s",
            root,
            query,
            api.key
        )

        if (!is.null(release_id))
            sprintf('%s&release_id=%s',url,release_id %>>% paste(collapse=";")) -> url        
        if (!is.null(limit))
            sprintf('%s&limit=%s',url,limit) -> url
        if (!is.null(tag_names))
            sprintf('%s&tag_names=%s',url,tag_names) -> url
        if (!is.null(exclude_tag_names))
            sprintf('%s&exclude_tag_names=%s',url,exclude_tag_names) -> url
        if (!is.null(tag_group_id))
            sprintf('%s&tag_group_id=%s',url,tag_group_id) -> url
        if (!is.null(search_text))
            sprintf('%s&search_text=%s',url,search_text) -> url
        
        getURL(
            url = url %>>%
            URLencode
        ) %>>%
        xmlTreeParse(
            useInternalNodes = TRUE
        ) -> l

        dt <- xml2dt(l) 

        return(dt)                
    }

    release.related_tags <- function(
        release_id = NULL,
        limit = 1000,
        tag_names = NULL,
        exclude_tag_names = NULL,
        tag_group_id = NULL,
        search_text = NULL
    ){
        if (is.null(release_id))
            stop('release_id is a required argument.')
        if (limit > 1000 | limit < 0)
            stop("Limit must be between 0 and 1000.")
        
        query = 'fred/release/related_tags'
        url = sprintf(
            "%s/%s?&api_key=%s",
            root,
            query,
            api.key
        )

        if (!is.null(release_id))
            sprintf('%s&release_id=%s',url,release_id %>>% paste(collapse=";")) -> url        
        if (!is.null(limit))
            sprintf('%s&limit=%s',url,limit) -> url
        if (!is.null(tag_names))
            sprintf('%s&tag_names=%s',url,tag_names) -> url
        if (!is.null(exclude_tag_names))
            sprintf('%s&exclude_tag_names=%s',url,exclude_tag_names) -> url
        if (!is.null(tag_group_id))
            sprintf('%s&tag_group_id=%s',url,tag_group_id) -> url
        if (!is.null(search_text))
            sprintf('%s&search_text=%s',url,search_text) -> url
        
        getURL(
            url = url %>>%
            URLencode
        ) %>>%
        xmlTreeParse(
            useInternalNodes = TRUE
        ) -> l

        dt <- xml2dt(l) 

        return(dt)                
    }

    release.sources <- function(
        release_id = NULL
    ){
        if (is.null(release_id))
            stop('release_id is a required argument.')
        
        query = 'fred/release/sources'
        url = sprintf(
            "%s/%s?&api_key=%s",
            root,
            query,
            api.key
        )

        if (!is.null(release_id))
            sprintf('%s&release_id=%s',url,release_id %>>% paste(collapse=";")) -> url        

        getURL(
            url = url %>>%
            URLencode
        ) %>>%
        xmlTreeParse(
            useInternalNodes = TRUE
        ) -> l

        dt <- xml2dt(l) 

        return(dt)                
    }             

    
    ## ---------------------------------------------------------------------- ##
    ##                             SERIES                                     ##
    ## ---------------------------------------------------------------------- ##
    series <- function(
        series_id = NULL
    ){
        if (is.null(series_id))
            stop('series_id is a required input.')
        
        query = 'fred/series'
        getURL(
            url = sprintf(
                "%s/%s?series_id=%s&api_key=%s",
                root,
                query,
                series_id,
                api.key
            ) %>>%
            URLencode
        ) %>>%
        xmlTreeParse(
            useInternalNodes = TRUE
        ) -> l

        dt <- xml2dt(l) 

        return(dt)      
    }

    series.categories <- function(
        series_id = NULL
    ){
        if (is.null(series_id))
            stop('series_id is a required input.')
        
        query = 'fred/series/categories'
        getURL(
            url = sprintf(
                "%s/%s?series_id=%s&api_key=%s",
                root,
                query,
                series_id,
                api.key
            ) %>>%
            URLencode
        ) %>>%
        xmlTreeParse(
            useInternalNodes = TRUE
        ) -> l

        dt <- xml2dt(l) 

        return(dt)                
    }

    series.observations <- function(
        series_id = NULL,
        limit = NULL,
        observation_start = NULL,       # YYYY-MM-DD
        observation_end = NULL,         # YYYY-MM-DD
        units = NULL,                   #'lin', 'chg', 'ch1', 'pch', 'pc1',
                                        #'pca', 'cch', 'cca', 'log'
        frequency = NULL,               # 'd', 'w', 'bw', 'm', 'q', 'sa', 'a',
                                        # 'wef', 'weth', 'wew', 'wetu', 'wem',
                                        # 'wesu', 'wesa', 'bwew', 'bwem'
        aggregation_method = NULL,      #'avg', 'sum', 'eop'
        output_type = NULL              #  '1', '2', '3', '4'
                                        # 1 = Observations by Real-Time Period
                                        # 2 = Observations by Vintage Date, All Observations
                                        # 3 = Observations by Vintage Date, New and Revised Observations Only
                                        # 4 = Observations, Initial Release Only
        
    ){
        if (is.null(series_id))
            stop('series_id is a required input.')

        query = 'fred/series/observations'
        
        url = sprintf(
            "%s/%s?series_id=%s&api_key=%s",
            root,
            query,
            series_id,
            api.key
        )
        
        if (!is.null(limit))
            sprintf('%s&limit=%s',url,limit) -> url

        if (!is.null(observation_start))
            sprintf('%s&observation_start=%s',url,observation_start) -> url

        if (!is.null(observation_end))
            sprintf('%s&observation_end=%s',url,observation_end) -> url

        if (!is.null(units))
            sprintf('%s&units=%s',url,units) -> url

        if (!is.null(frequency))
            sprintf('%s&frequency=%s',url,frequency) -> url

        if (!is.null(aggregation_method))
            sprintf('%s&aggregation_method=%s',url,aggregation_method) -> url

        if (!is.null(output_type))
            sprintf('%s&output_type=%s',url,output_type) -> url        
                
        getURL(
            url = url %>>% URLencode
        ) %>>%
        xmlTreeParse(
            useInternalNodes = TRUE
        ) -> l

        dt <- xml2dt(l) 

        return(dt)        
    }

    series.release <- function(
        series_id = NULL
    ){
        if (is.null(series_id))
            stop('series_id is a required input.')
        
        query = 'fred/series/release'
        getURL(
            url = sprintf(
                "%s/%s?series_id=%s&api_key=%s",
                root,
                query,
                series_id,
                api.key
            ) %>>%
            URLencode
        ) %>>%
        xmlTreeParse(
            useInternalNodes = TRUE
        ) -> l

        dt <- xml2dt(l) 

        return(dt)                
    }

    series.search <- function(
        text = NULL,
        search_type = 'full_text', # c('full_text','series_id')
        order_by = 'popularity',   #  'search_rank', 'series_id', 'title',
                                        #  'units', 'frequency',
                                        #  'seasonal_adjustment', 'realtime_start',
                                        #  'realtime_end', 'last_updated',
                                        #  'observation_start', 'observation_end', 'popularity'.
        limit = 100
    ){
        if (is.null(text))
            stop('Specify a search string.')
        
        query = 'fred/series/search'
        getURL(
            url = sprintf(
                "%s/%s?search_text=%s&search_type=%s&order_by=%s&limit=%s&api_key=%s",
                root,
                query,
                text,
                search_type,
                order_by,
                limit,
                api.key
            ) %>>%
            URLencode
        ) %>>%
        xmlTreeParse(
            useInternalNodes = TRUE
        ) -> l

        dt <- xml2dt(l) 

        return(dt)
    }

    series.search.tags <- function(
        series_search_text = NULL
    ){
        if (is.null(series_search_text))
            stop('series_search_text and tag_names are required inputs for this function.')
        
        query = 'fred/series/search/tags'
        getURL(
            url = sprintf(
                "%s/%s?series_search_text=%s&api_key=%s",
                root,
                query,
                series_search_text,
                api.key
            ) %>>%
            URLencode
        ) %>>%
        xmlTreeParse(
            useInternalNodes = TRUE
        ) -> l

        dt <- xml2dt(l) 

        return(dt)      
    }

    series.search.related_tags <- function(
        series_search_text = NULL,
        tag_names = NULL
    ){
        if (is.null(series_search_text) | is.null(tag_names))
            stop('series_search_text and tag_names are required inputs for this function.')
        
        query = 'fred/series/search/related_tags'
        getURL(
            url = sprintf(
                "%s/%s?series_search_text=%s&tag_names=%s&api_key=%s",
                root,
                query,
                series_search_text,
                tag_names %>>% paste(collapse = ';'),
                api.key
            ) %>>%
            URLencode
        ) %>>%
        xmlTreeParse(
            useInternalNodes = TRUE
        ) -> l

        dt <- xml2dt(l) 

        return(dt)      
    }

    series.tags <- function(
        series_id = NULL
    ){
        if (is.null(series_id))
            stop('series_id is a required input.')
        
        query = 'fred/series/tags'
        getURL(
            url = sprintf(
                "%s/%s?series_id=%s&api_key=%s",
                root,
                query,
                series_id,
                api.key
            ) %>>%
            URLencode
        ) %>>%
        xmlTreeParse(
            useInternalNodes = TRUE
        ) -> l

        dt <- xml2dt(l) 

        return(dt)                
    }

    series.updates <- function(){
        query = 'fred/series/updates'
        getURL(
            url = sprintf(
                "%s/%s?api_key=%s",
                root,
                query,
                api.key
            ) %>>%
            URLencode
        ) %>>%
        xmlTreeParse(
            useInternalNodes = TRUE
        ) -> l

        dt <- xml2dt(l) 

        return(dt)                
    }

    series.vintagedates <- function(
        series_id = NULL
        ## realtime_star = NULL,
        ## realtime_end = NULL,
        ## limit = NULL,
        ## offset = NULL,
        ## sort_order = NULL
    ){
        if (is.null(series_id))
            stop('series_id is a required input.')
        
        query = 'fred/series/vintagedates'
        getURL(
            url = sprintf(
                "%s/%s?series_id=%s&api_key=%s",
                root,
                query,
                series_id,
                api.key
            ) %>>%
            URLencode
        ) %>>%
        xmlTreeParse(
            useInternalNodes = TRUE
        ) -> l

        l %>>%
        xmlRoot %>>%
        xmlToDataFrame %>>%
        as.data.table %>>%
        rename(
            vintage = text
        ) ->
            dt

        return(dt)
    }

    ## ---------------------------------------------------------------------- ##
    ## SOURCES                                                                ##
    ## ---------------------------------------------------------------------- ##
    sources <- function(
        limit = 1000
    ){
        if (limit > 1000 | limit < 0)
            stop("Limit must be between 0 and 1000.")
        
        query = 'fred/sources'
        url = sprintf(
            "%s/%s?&api_key=%s",
            root,
            query,
            api.key
        )

        if (!is.null(limit))
            sprintf('%s&limit=%s',url,limit) -> url        
        
        getURL(
            url = url %>>%
            URLencode
        ) %>>%
        xmlTreeParse(
            useInternalNodes = TRUE
        ) -> l

        dt <- xml2dt(l) 

        return(dt)                
    }

    source <- function(
        source_id = NULL
    ){
        if (is.null(source_id))
            stop("source_id is a required argument.")
        
        query = 'fred/source'
        url = sprintf(
            "%s/%s?&api_key=%s",
            root,
            query,
            api.key
        )

        if (!is.null(source_id))
            sprintf('%s&source_id=%s',url,source_id) -> url        
        
        getURL(
            url = url %>>%
            URLencode
        ) %>>%
        xmlTreeParse(
            useInternalNodes = TRUE
        ) -> l

        dt <- xml2dt(l) 

        return(dt)                
    }

    source.releases <- function(
        source_id = NULL,
        limit = 1000
    ){
        if (is.null(source_id))
            stop("source_id is a required argument.")
        
        query = 'fred/source/releases'
        url = sprintf(
            "%s/%s?&api_key=%s",
            root,
            query,
            api.key
        )

        if (!is.null(source_id))
            sprintf('%s&source_id=%s',url,source_id) -> url
        if (!is.null(limit))
            sprintf('%s&limit=%s',url,limit) -> url               
        
        getURL(
            url = url %>>%
            URLencode
        ) %>>%
        xmlTreeParse(
            useInternalNodes = TRUE
        ) -> l

        dt <- xml2dt(l) 

        return(dt)                
    }    
    
    ## ---------------------------------------------------------------------- ##
    ## TAGS                                                                   ##
    ## ---------------------------------------------------------------------- ##
    tags <- function(
        tag_names = NULL,
        exclude_tag_names = NULL,
        tag_group_id = NULL,
        search_text = NULL,
        limit = NULL
    ){        
        query = 'fred/tags'
        url = sprintf(
            "%s/%s?api_key=%s",
            root,
            query,
            api.key
        )

        if (!is.null(tag_names))
            sprintf('%s&tag_names=%s',url,tag_names) -> url
        if (!is.null(exclude_tag_names))
            sprintf('%s&exclude_tag_names=%s',url,exclude_tag_names) -> url
        if (!is.null(tag_group_id))
            sprintf('%s&tag_group_id=%s',url,tag_group_id) -> url
        if (!is.null(search_text))
            sprintf('%s&search_text=%s',url,search_text) -> url
        if (!is.null(limit))
            sprintf('%s&limit=%s',url,limit) -> url        
        
        getURL(
            url = url %>>%
            URLencode
        ) %>>%
        xmlTreeParse(
            useInternalNodes = TRUE
        ) -> l

        dt <- xml2dt(l) 

        return(dt)                
    }


    related_tags <- function(
        tag_names = NULL,
        exclude_tag_names = NULL,
        tag_group_id = NULL,            # 'freq', 'gen', 'geo', 'geot', 'rls',
                                        # 'seas', 'src'
        search_text = NULL,
        limit = 1000
    ){
        if (is.null(tag_names) | is.null(exclude_tag_names))
            stop('At least one of the tag_names and exclude_tag_names parameters are required to be set')
        if (limit > 1000 | limit < 0)
            stop("Limit must be between 0 and 1000.")
        
        query = 'fred/related_tags'
        url = sprintf(
            "%s/%s?&api_key=%s",
            root,
            query,
            api.key
        )

        if (!is.null(tag_names))
            sprintf('%s&tag_names=%s',url,tag_names %>>% paste(collapse = ";")) -> url
        if (!is.null(exclude_tag_names))
            sprintf('%s&exclude_tag_names=%s',url,exclude_tag_names %>>% paste(collapse = ";")) -> url
        if (!is.null(tag_group_id))
            sprintf('%s&tag_group_id=%s',url,tag_group_id) -> url
        if (!is.null(search_text))
            sprintf('%s&search_text=%s',url,search_text) -> url
        if (!is.null(limit))
            sprintf('%s&limit=%s',url,limit) -> url        
        
        getURL(
            url = url %>>%
            URLencode
        ) %>>%
        xmlTreeParse(
            useInternalNodes = TRUE
        ) -> l

        dt <- xml2dt(l) 

        return(dt)                
    }

    tags.series <- function(
        tag_names = NULL,
        exclude_tag_names = NULL,
        limit = 1000
    ){
        if (is.null(tag_names))
            stop('At least one of the tag_names and exclude_tag_names parameters are required to be set')
        if (limit > 1000 | limit < 0)
            stop("Limit must be between 0 and 1000.")
        
        query = 'fred/tags/series'
        url = sprintf(
            "%s/%s?&api_key=%s",
            root,
            query,
            api.key
        )

        if (!is.null(tag_names))
            sprintf('%s&tag_names=%s',url,tag_names %>>% paste(collapse = ";")) -> url
        if (!is.null(exclude_tag_names))
            sprintf('%s&exclude_tag_names=%s',url,exclude_tag_names %>>% paste(collapse = ";")) -> url
        if (!is.null(limit))
            sprintf('%s&limit=%s',url,limit) -> url        
        
        getURL(
            url = url %>>%
            URLencode
        ) %>>%
        xmlTreeParse(
            useInternalNodes = TRUE
        ) -> l

        dt <- xml2dt(l) 

        return(dt)                
    }    
    
    ## Return
    o <- list(
        getChildrenList = getChildrenList,
        cetegory = category,
        category.children = category.children,
        category.related = category.related,
        category.series = category.series,
        category.tags = category.tags,
        category.related_tags = category.related_tags,
        releases = releases,
        releases.dates = releases.dates,
        release = release,
        release.dates = release.dates,
        release.series = release.series,
        release.sources = release.sources,
        release.tags = release.tags,
        release.related_tags = release.related_tags,
        series = series,
        series.categories = series.categories,
        series.observations = series.observations,
        series.search = series.search,
        series.search.tags = series.search.tags,
        series.search.related_tags = series.search.related_tags,
        series.tags = series.tags,
        series.updates = series.updates,
        series.vintagedates = series.vintagedates,
        sources = sources,
        source = source,
        source.releases = source.releases,
        tags = tags,
        related_tags = related_tags,
        tags.series = tags.series
    )

    return(structure(o,
                     class = 'FredR'))
}
