
<!-- README.md is generated from README.Rmd. Please edit that file -->
sugrrants
=========

[![Travis-CI Build Status](https://travis-ci.org/earowang/sugrrants.svg?branch=master)](https://travis-ci.org/earowang/sugrrants) [![Coverage Status](https://img.shields.io/codecov/c/github/earowang/sugrrants/master.svg)](https://codecov.io/github/earowang/sugrrants?branch=master) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/sugrrants)](https://cran.r-project.org/package=sugrrants) [![Downloads](http://cranlogs.r-pkg.org/badges/sugrrants?color=brightgreen)](https://cran.r-project.org/package=sugrrants)

Overview
--------

The goal of *sugrrants* is to provide supporting graphs with R for analysing time series data. It aims to fit into the *tidyverse* and grammar of graphics framework for handling temporal data.

Installation
------------

You could install the stable version on CRAN:

``` r
install.packages("sugrrants")
```

You could also install the development version from Github using:

``` r
# install.packages("devtools")
devtools::install_github("earowang/sugrrants", build_vignettes = TRUE)
```

Usage
-----

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
#>              Date_Time  Year   Month Mdate    Day  Time Sensor_ID
#>  *              <dttm> <int>   <ord> <int>  <ord> <int>     <int>
#>  1 2016-01-01 00:00:00  2016 January     1 Friday     0         9
#>  2 2016-01-01 01:00:00  2016 January     1 Friday     1         9
#>  3 2016-01-01 02:00:00  2016 January     1 Friday     2         9
#>  4 2016-01-01 03:00:00  2016 January     1 Friday     3         9
#>  5 2016-01-01 04:00:00  2016 January     1 Friday     4         9
#>  6 2016-01-01 05:00:00  2016 January     1 Friday     5         9
#>  7 2016-01-01 06:00:00  2016 January     1 Friday     6         9
#>  8 2016-01-01 07:00:00  2016 January     1 Friday     7         9
#>  9 2016-01-01 08:00:00  2016 January     1 Friday     8         9
#> 10 2016-01-01 09:00:00  2016 January     1 Friday     9         9
#> # ... with 8,770 more rows, and 6 more variables: Sensor_Name <chr>,
#> #   Hourly_Counts <int>, Weekend <chr>, Date <date>, .Time <dbl>,
#> #   .Hourly_Counts <dbl>
```

``` r
p <- calendar_df %>%
  ggplot(aes(x = .Time, y = .Hourly_Counts, group = Date, colour = Weekend)) +
  geom_line() +
  theme(legend.position = "bottom")
prettify(p)
```

![](man/figure/calendar-plot-1.png)

Miscellaneous
-------------

The acronym of *sugrrants* is **SU**pporting **GR**aphs with **R** for **AN**alysing **T**ime **S**eries, pronounced as "sugar ants" that are a species of ant endemic to Australia.
