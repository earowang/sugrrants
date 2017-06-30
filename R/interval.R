## NOTES:
## (1) I use "Frequency" as class to avoid clashing with lubridate "Interval"
## class, and also be consistent with ts object that uses "frequency".
## (2) Why use R6 instead of S3? Time interval could have so many subclasses
## including HMS, Day, Week, Month, Quarter, Year. It seems that it follows S3
## method from specific to more general, but it's actually not. A tsibble with
## an hourly interval could be aggregated to yearly data, jumping from HMS to
## Year. S3 would keep searching until the right class. Without specifying a
## certain order of those subclasses, R6 is more flexibile in this way. I don't
## need to define methods for separate classes.
## (3) "Frequency" serves like a parent class rather than a general class.
## (4) As R6 is mutable, should be more careful of possible side effects.
tsibble_interval <- R6Class(
  "Frequency",
  private = list(
    data = NULL
  ),
  public = list(
    present = function() {
      not_zero <- !map_lgl(private$data, function(x) x == 0)
      output <- private$data[not_zero]
      return(output)
    },
    display = function() {
      output <- self$present()
      return(paste0(flatten_dbl(output), toupper(names(output))))
    },
    print = function(...) {
      cat(self$display(), "\n")
      invisible(self)
    }
  )
)

tsibble_year <- R6Class(
  "Year",
  inherit = tsibble_interval,
  private = list(data = NULL),
  public = list(
    initialize = function(year = 0) {
      private$data$year <- year
    }
  )
)

tsibble_quarter <- R6Class(
  "Quarter",
  inherit = tsibble_interval,
  private = list(data = NULL),
  public = list(
    initialize = function(quarter = 0) {
      private$data$quarter <- quarter
    }
  )
)

tsibble_month <- R6Class(
  "Month",
  inherit = tsibble_interval,
  private = list(data = NULL),
  public = list(
    initialize = function(month = 0) {
      private$data$month <- month
    }
  )
)

tsibble_day <- R6Class(
  "Day",
  inherit = tsibble_interval,
  private = list(data = NULL),
  public = list(
    initialize = function(day = 0) {
      private$data$day <- day
    }
  )
)

tsibble_hms <- R6Class(
  "HMS",
  inherit = tsibble_interval,
  private = list(data = NULL),
  public = list(
    initialize = function(hour = 0, minute = 0, second = 0) {
      private$data$hour <- hour
      private$data$minute <- minute
      private$data$second <- second
    }
  )
)
