## NOTES:
## (1) I use "Frequency" as class to avoid clashing with lubridate "Interval"
## class, and also be consistent with ts object that uses "frequency".
## (2) Why use R6 instead of S3? Time interval could have so many subclasses
## including HMS, Day, Week, Month, Quarter, Year. It seems that it follows S3
## method from specific to more general, but it's actually not. A tsibble with
## an hourly interval could be aggregated to yearly data, jumping from HMS to
## Year. S3 would keep searching until the right class. Without specifying a
## certain order of those subclasses, R6 is more flexibile in this way.
## (3) "Frequency" serves like a parent class rather than a general class.
## (4) As R6 is mutable, should be more careful of possible side effects.
tsibble_interval <- R6::R6Class(
  "Frequency",
  private = list(
    data = NULL,
    class = NULL
  ),
  active = list(
    get_interval = function() {
      return(private$data)
    },
    get_class = function() {
      return(private$class)
    }
  ),
  public = list(
    initialize = function(
      year = 0, quarter = 0, month = 0, week = 0,
      day = 0, hour = 0, minute = 0, second = 0
    ) {
      private$data$year <- year
      private$data$quarter <- quarter
      private$data$month <- month
      private$data$week <- week
      private$data$day <- day
      private$data$hour <- hour
      private$data$minute <- minute
      private$data$second <- second
      self$update_class()
    },
    present = function() {
      not_zero <- !map_lgl(private$data, function(x) x == 0)
      output <- private$data[not_zero]
      return(output)
    },
    update_class = function() {
      output <- self$present()
      names_output <- names(output)
      if ("year" %in% names_output) {
        private$class <- "Year"
      } else if ("quarter" %in% names_output) {
        private$class <- "Quarter"
      } else if ("month" %in% names_output) {
        private$class <- "Month"
      } else if ("week" %in% names_output) {
        private$class <- "Week"
      } else if ("day" %in% names_output) {
        private$class <- "Day"
      } else if (is_empty(names_output)) {
        private$class <- NULL
      } else {
        private$class <- "HMS"
      }
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

