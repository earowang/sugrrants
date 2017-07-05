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
      return(paste0(rlang::flatten_dbl(output), toupper(names(output))))
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

# Number of time units
gen_interval <- function(date) {
  UseMethod("gen_interval")
}

gen_interval.default <- function(date) {
  output <- min_interval(date) # num of years
  return(output)
}

gen_interval.POSIXt <- function(date) {
  dttm <- as.numeric(date)
  output <- min_interval(dttm) # num of seconds
  return(output)
}

gen_interval.Date <- function(date) {
  date <- as.numeric(date)
  output <- min_interval(date) # num of days
  return(output)
}

gen_interval.yearmon <- function(date) {
  # num of months
  mon <- as.numeric(date)
  output <- ceiling(min_interval(mon) * 12)
  return(output)
}

gen_interval.yearqtr <- function(date) {
  # num of quarters
  qtr <- as.numeric(date)
  output <- ceiling(min_interval(qtr) * 4)
  return(output)
}

# Assume date is regularly spaced
# R6Class to manage tsibble interval, although the printing info is character.
pull_interval <- function(date) {
  UseMethod("pull_interval")
}

pull_interval.default <- function(date) {
  nyrs <- gen_interval.default(date)
  output <- tsibble_year$new(year = nyrs)
  return(output)
}

pull_interval.POSIXt <- function(date) {
  nhms <- gen_interval.POSIXt(date)
  period <- period2list(nhms)
  output <- tsibble_hms$new(
    hour = period$hour, minute = period$minute, second = period$second
  )
  return(output)
}

pull_interval.Date <- function(date) {
  ndays <- gen_interval.Date(date)
  output <- tsibble_day$new(day = ndays)
  return(output)
}

pull_interval.yearmon <- function(date) {
  nmonths <- gen_interval.yearmon(date)
  output <- tsibble_month$new(month = nmonths)
  return(output)
}

pull_interval.yearqtr <- function(date) {
  nqtrs <- gen_interval.yearqtr(date)
  output <- tsibble_quarter$new(quarter = nqtrs)
  return(output)
}

## helper function
period2list <- function(x) {
  output <- seconds_to_period(x)
  return(list(
    year = output$year, month = output$month, day = output$day,
    hour = output$hour, minute = output$minute, second = output$second
  ))
} 

min_interval <- function(date) {
  return(min(abs(diff(as.numeric(date), na.rm = TRUE))))
}

support_cls <- function() {
  return(c(
    "Date", "POSIXt", "yearmon", "yearqtr", "integer", "numeric"
  ))
}
