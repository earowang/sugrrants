# sugrrants 0.2.5

* Renamed dataset `pedestrian` to `hourly_peds` to avoid conflict with `tsibble::pedestrian`. (#18)

# sugrrants 0.2.4

* Started to issue an warning when the deprecated argument `sunday` is present in `frame_calendar()`.
* Fixed week day labels in `prettify()` inconsistent with user-specified `week_start`.
* Added `...` to `frame_calendar()` S3 generic.

# sugrrants 0.2.3

This is a patch release for the changes in **tsibble**.

# sugrrants 0.2.2

This is a maintenance release.

* `facet_calendar()` now accepts calls in the argument `date`. (#14)

# sugrrants 0.2.1

This is a patch release for the changes in **tsibble**.

# sugrrants 0.2.0

* Added a new faceting method `facet_calendar()`.
* Added new argument `week_start` into `frame_calendar()`
* Soft-deprecated `sunday` argument in `frame_calendar()` and will start issuing warnings in the next release.

# sugrrants 0.1.6

* Removed dependency on `tidyr`.
* Improved default margin between monthly grids in `frame_calendar()`.

# sugrrants 0.1.5

* Avoid strong dependency on `tsibble` & `readr`.

# sugrrants 0.1.4

* Retired `au_holiday()`, since tsibble offers a complete version in `holiday_aus()`.
* Made `prettify()` S3 generic, and added support for plotly object. (#9)
* Removed **purrr** dependency.
* Added **plotly** as Suggests.

# sugrrants 0.1.3

* Deprecated `au_holiday()`, as a complete function `holiday_aus()` is available in the **tsibble** package.
* Fixed `grouped_df` issue to be compatible with ggplot2 v3.0.0.

# sugrrants 0.1.2

* Added a new argument `margin = NULL` to allow users for margin adjustment between month panels.
* Fixed variable scoping issues in `frame_calendar()`.
* `frame_calendar()` added support to tsibble or `tbl_ts`.
* Added **tsibble** as Imports.
* Improved `frame_calendar()` in conjunction with `group_by()` for incomplete time series (#3).
* Fixed y axis limits only depending on the first variable when using multiple variables (#5).

# sugrrants 0.1.1

## Dropped functions

* dropped an internal function `wday2()` since the lubridate v1.7.1 added a new argument `week_start` in the `wday()`.

## Improvement

* `frame_calendar()`: arguments `x` and `y` support factor and hms class.
* Added a note on AFL holiday for the documentation of `au_holiday(state = "VIC")`.

# sugrrants 0.1.0

* Initial release
* Added a `NEWS.md` file to track changes to the package.
* Added the `frame_calendar()` function to compute calendar grids for visual representation of temporal data, coupled with a vignette.
* Added the `geom_acf()` and `stat_acf()` functions to incorporate ACF/PACF plots into the grammar of graphics framework.
