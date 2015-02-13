#FredR: R Interface to the Federal Reserve Economic Data API

This package provides an interface to the Federal Reserve Economic Data (FRED) API. FRED covers 240,000 US and international macroeconomic time series from 77 sources (including the World Bank,  OECD, and BIS).

FredR has been designed in a way that gives a user a complete interface to all available functionalities of the API. Currently,  the package is still poorly documented (improving on this will be the next step). However, the naming conventions used in the package map one-to-one to the official API calls, so one can for now use the official API documentation (available [**here**](http://api.stlouisfed.org/docs/fred/?utm_source=research&utm_medium=website&utm_campaign=data-tools)) to see the meaning of particular functions and parameters.

##Installation
FredR is currently only available on Github, so you will need Hadley's **devtools** in order to install it:

```r
devtools::install_github(jcizel/FredR)
```

## Quick Demonstration

In order to use the API, you first need to create an account here: http://api.stlouisfed.org/api_key.html. Once you have the account, copy-paste your  API key and supply it is a string argument to FredR function, like so:

```r
api.key = '...'  # substitute ... with your API key
```

Next,  you need to initialize FredR:

```r
fred <- FredR(api.key)
```

Call to FredR generates a list of functions each of which performs a specific type of query to the API. Each functions returns a data.table object containing the result of the query.

Let's view all the available functions in FredR:

```r
str(fred,1)
```

Ok, let's get to work. Say, that we want to find all series, whose name contains a string 'GDP':

```r
gdp.series <- fred$series.search("GDP")
```

`gdp` is now a `data.table` containing all available GDP series and their properties.

To see which one to use for further analysis, one can dig deeper:

```r
library(pipeR)
library(dplyr)

gdp.series %>>%
select(
    id,
    title,
    observation_start,
    observation_end,
    popularity
) %>>%
arrange(
    desc(as.numeric(popularity))
)
```

Let's say that we decide to download the time series data for the series with `id =
'GDPC1'` (Real Gross Domestic Product in the U.S.):

```r
gdp <- fred$series.observations(series_id = 'GDPC1')
```

Plot the result:

```r
gdp %>>%
select(
    date,
    value
) %>>%
mutate(
    date = as.Date(date),
    value = as.numeric(value)
) ->
    dt

require(ggplot2)
qplot(data = dt, x = date, y = value, geom = 'line')
```
