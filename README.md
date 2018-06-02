
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sugrrants

[![Travis-CI Build
Status](https://travis-ci.org/earowang/sugrrants.svg?branch=master)](https://travis-ci.org/earowang/sugrrants)
[![Coverage
Status](https://img.shields.io/codecov/c/github/earowang/sugrrants/master.svg)](https://codecov.io/github/earowang/sugrrants?branch=master)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/sugrrants)](https://cran.r-project.org/package=sugrrants)
[![Downloads](http://cranlogs.r-pkg.org/badges/sugrrants?color=brightgreen)](https://cran.r-project.org/package=sugrrants)

The goal of *sugrrants* is to provide supporting graphs with R for
analysing time series data. It aims to fit into the *tidyverse* and
grammar of graphics framework for handling temporal data.

## Installation

You could install the stable version on CRAN:

``` r
install.packages("sugrrants")
```

You could also install the development version from Github using:

``` r
# install.packages("devtools")
devtools::install_github("earowang/sugrrants", build_vignettes = TRUE)
```

## Usage

### Calendar-based graphics

``` r
library(dplyr)
library(sugrrants)

calendar_df <- pedestrian %>%
  filter(Sensor_ID == 9, Year == 2016) %>%
  mutate(
    Weekend = if_else(Day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
  ) %>%
  frame_calendar(
    x = Time, y = Hourly_Counts, date = Date, calendar = "monthly"
  )
calendar_df
#> # A tibble: 8,780 x 13
#>    Date_Time           Date        Year Month  Mdate Day    Time Sensor_ID
#>  * <dttm>              <date>     <int> <ord>  <int> <ord> <int>     <int>
#>  1 2016-01-01 00:00:00 2016-01-01  2016 Janua…     1 Frid…     0         9
#>  2 2016-01-01 01:00:00 2016-01-01  2016 Janua…     1 Frid…     1         9
#>  3 2016-01-01 02:00:00 2016-01-01  2016 Janua…     1 Frid…     2         9
#>  4 2016-01-01 03:00:00 2016-01-01  2016 Janua…     1 Frid…     3         9
#>  5 2016-01-01 04:00:00 2016-01-01  2016 Janua…     1 Frid…     4         9
#>  6 2016-01-01 05:00:00 2016-01-01  2016 Janua…     1 Frid…     5         9
#>  7 2016-01-01 06:00:00 2016-01-01  2016 Janua…     1 Frid…     6         9
#>  8 2016-01-01 07:00:00 2016-01-01  2016 Janua…     1 Frid…     7         9
#>  9 2016-01-01 08:00:00 2016-01-01  2016 Janua…     1 Frid…     8         9
#> 10 2016-01-01 09:00:00 2016-01-01  2016 Janua…     1 Frid…     9         9
#> # ... with 8,770 more rows, and 5 more variables: Sensor_Name <chr>,
#> #   Hourly_Counts <int>, Weekend <chr>, .Time <dbl>, .Hourly_Counts <dbl>
```

``` r
p <- calendar_df %>%
  ggplot(aes(x = .Time, y = .Hourly_Counts, group = Date, colour = Weekend)) +
  geom_line() +
  theme(legend.position = "bottom")
prettify(p, label.padding = unit(0.08, "lines"))
```

![](man/figure/calendar-plot-1.png)<!-- -->

## Google Summer of Code 2017

This package is part of the project—[Tidy data structures and visual
methods to support exploration of big temporal-context
data](https://summerofcode.withgoogle.com/projects/#4790455121215488),
which has been participated in Google Summer of Code 2017 (gsoc), for [R
project for statistical computing](https://www.r-project.org).

A new function `frame_calendar()`
\[[here](https://github.com/earowang/sugrrants/blob/master/R/frame-calendar.R)
and
[here](https://github.com/earowang/sugrrants/blob/master/R/calendar-fun.R)\]
in the **sugrrants** package has been developed and documented for
calendar-based graphics. I have also written a vignette
\[[source](https://github.com/earowang/sugrrants/blob/master/vignettes/frame-calendar.Rmd)
and [reader
view](http://pkg.earo.me/sugrrants/articles/frame-calendar.html)\],
which introduces and demonstrates the usage of the `frame_calendar()`
function. [Many unit
tests](https://github.com/earowang/sugrrants/blob/master/tests/testthat/test-calendar.R)
have been carried out to ensure the expected performance of this
function. The function implements non-standard evaluation and highlights
the [tidy
evaluation](http://rlang.tidyverse.org/articles/tidy-evaluation.html) in
action. The initial release (v0.1.0) of the package has been published
on [CRAN](https://CRAN.R-project.org/package=sugrrants) during the gsoc
summer time.

I have initialised a new R package
[**tsibble**](https://github.com/earowang/tsibble) for tidy temporal
data, as part of the project. The `tsibble()` function constructs a new
`tbl_ts` class for temporal data, and the `as_tsibble()` helps to
convert a few `ts` objects into the `tbl_ts` class. Some key verbs
(generics) from the **dplyr** package, such as `mutate()`,
`summarise()`, `filter()`, have been defined and developed for the
`tbl_ts` data class. The **tsibble** package was highly experimental
over the period of the gsoc
\[[commits](https://github.com/earowang/tsibble/commit/aba1cfc2eec88966c43232fe5d249522f88e1e27)\],
and these functions are very likely to be changed or improved in the
future.

A new package [**rwalkr**](https://github.com/earowang/rwalkr) has been
created and released on
[CRAN](https://cran.r-project.org/package=rwalkr) during the gsoc
summer. This package provides API to Melbourne pedestrian sensor data
and arrange the data in tidy temporal data form. Two functions including
[`walk_melb()`](https://github.com/earowang/rwalkr/blob/master/R/scrape.R)
and
[`shine_melb()`](https://github.com/earowang/rwalkr/blob/master/R/shiny.R),
have been written and documented as the v0.1.0 and v0.2.0 releases on
CRAN. The majority of the code for the function
[`run_melb()`](https://github.com/earowang/rwalkr/blob/master/R/soda.R)
has been done, but the interface needs improving after the gsoc.

## Miscellaneous

The acronym of *sugrrants* is **SU**pporting **GR**aphs with **R** for
**AN**alysing **T**ime **S**eries, pronounced as “sugar ants” that are a
species of ant endemic to Australia.
