# sugrrants 0.1.0.9000

## Dropped functions

* dropped an internal function `wday2()` since the lubridate v1.7.1 added a new argument `week_start` in the `wday()`.

## Improvement

* `frame_calendar()`: arguments `x` and `y` support factor and hms class.

# sugrrants 0.1.0

* Initial release
* Added a `NEWS.md` file to track changes to the package.
* Added the `frame_calendar()` function to compute calendar grids for visual representation of temporal data, coupled with a vignette.
* Added the `geom_acf()` and `stat_acf()` functions to incorporate ACF/PACF plots into the grammar of graphics framework.
