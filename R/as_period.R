#' Get seasons component of a date-time
#'
#' @param x A date-time object.
#' @param hemisphere A character vector that specifies the hemisphere. "N" refers
#'    to the Northern hemisphere, and "S" to the Souther hemisphere.
#' @param definition A character vector that defines the season range. "meterological"
#'    is the only one supported at the stage.
#'
#' @return The season of \code{x} as character string.
#'
#' @seealso as_season
#'
#' @author Earo Wang
#' @references 
#'    Barnett, A. G. and Dobson A. J. (2010) Analysing seasonal health data. Springer.
#' 
#' @export
#'
#' @examples
#'    x <- as.POSIXct(c("2014-01-31", "2015-07-31", "2016-10-31"))
#'    season(x, hemisphere = "S") # I'm in Australia
#'
season <- function(x, hemisphere = c("N", "S"), 
                   definition = c("meterological", "astronomical", 
                                  "midsolstice")) {
  hemisphere <- match.arg(hemisphere)
  definition <- match.arg(definition)
  month_x <- month(x)
  if (hemisphere == "N") {
    season_x <- case_when(
      month_x %in% 3:5 ~ "Spring",
      month_x %in% 6:8 ~ "Summer",
      month_x %in% 9:11 ~ "Autumn",
      month_x %in% c(12, 1:2) ~ "Winter"
    )
  } else {
    season_x <- case_when(
      month_x %in% 3:5 ~ "Autumn",
      month_x %in% 6:8 ~ "Winter",
      month_x %in% 9:11 ~ "Spring",
      month_x %in% c(12, 1:2) ~ "Summer"
    )
  }
  return(season_x)
}

#' @rdname as_month
#'
#' @param hemisphere A character vector that specifies the hemisphere. "N" refers
#'    to the Northern hemisphere, and "S" to the Souther hemisphere.
#' @param definition A character vector that defines the season range. "meterological"
#'    is the only one supported at the stage.
#'
#' @references 
#'    Barnett, A. G. and Dobson A. J. (2010) Analysing seasonal health data. Springer.
#'
#' @export
as_season <- function(x, hemisphere = "N", definition = "meterological",
                      date_format = TRUE) {
  hemisphere <- match.arg(hemisphere, c("N", "S"))
  definition <- match.arg(
    definition, c("meterological", "astronomical", "midsolstice")
  )

  year_x <- year(x)
  season_x <- season(x, hemisphere = hemisphere, definition = definition)
  if (date_format) {
    if (hemisphere == "N") {
      season_date <- case_when(
        season_x == "Spring" ~ "03-01",
        season_x == "Summer" ~ "06-01",
        season_x == "Autumn" ~ "09-01",
        season_x == "Winter" ~ "12-01"
      )
    } else {
      season_date <- case_when(
        season_x == "Autumn" ~ "03-01",
        season_x == "Winter" ~ "06-01",
        season_x == "Spring" ~ "09-01",
        season_x == "Summer" ~ "12-01"
      )
    }
    output <- ymd(paste(year_x, season_date, sep = "-"))
  } else {
    output <- paste(year_x, season_x, sep = "-")
  }
  return(output)
}

#' @name as_month
#' @aliases as_season
#' @aliases as_week
#'
#' @title Extract the calendar period components from a date-time
#'
#' @description Extract the calendar period components from a date-time
#'
#' @param x A date-time object.
#' @param date_format Logical. If TRUE, it returns a date format object; otherwise
#'    a character string.
#'
#' @return The period of \code{x} as date format or character string.
#'
#' @author Earo Wang
#' 
#' @rdname as_month
#' @export
#'
#' @examples
#'    x <- as.POSIXct(c("2014-01-31", "2015-07-31", "2016-10-31"))
#'    as_month(x)
#'
# apply to monthly data
# x takes datetime stamp and return the year-month-first of day
as_month <- function(x, date_format = TRUE) {
  year_x <- year(x)
  if (date_format) {
    month_x <- formatC(month(x), width = 2, flag = "0")
    output <- ymd(paste0(year_x, month_x, "01"))
  } else {
    month_x <- month(x, label = TRUE, abbr = TRUE)
    output <- paste(year_x, month_x, "-")
  }
  return(output)
}

#' @rdname as_month
#' @export
# apply to weekly data
# x takes datetime stamp and return the first date of the week
as_week <- function(x, date_format = TRUE) {
  wday_x <- wday(x, label = FALSE, abbr = FALSE)
  if (date_format) {
    diff_wday <- if_else(wday_x == 1, -6, 2 - wday_x) # find first date in the week
    output <- as_date(x + days(diff_wday))
  } else {
    chr_x <- paste0("W", formatC(wday_x, width = 2, flag = 0))
    year_x <- year(x)
    output <- paste(year_x, chr_x, sep = "-")
  }
  return(output)
}

#' @title Get weekdays component of a date-time
#'
#' @description A wrapper of \code{lubridate::wday()}, but allows to define the
#'    starting weekday.
#'
#' @param x A date-time object.
#' @param label Logical. If TRUE, it displays the day of the week as an ordered
#'    character strings; otherwise, an integer returns
#' @param abbr Logical. If TRUE, it returns an abbreviated version the label.
#'
#' @return The day of the week as a number (Monday is 1) or an ordered factor
#'    (Monday is first).
#'
#' @seealso \link[lubridate]{wday}
#'
#' @author Earo Wang
#' 
#' @export
#'
#' @examples
#'    x <- as.POSIXct(c("2014-01-31", "2015-07-31", "2016-10-31"))
#'    wday2(x, label = TRUE)
#'
# Allow reorder the wday levels (Starting from Monday by default)
wday2 <- function(x, label = FALSE, abbr = TRUE) {
  wdays <- wday(x, label = label, abbr = abbr)
  if (label) {
    levels_wdays <- levels(wdays)
    if (abbr) {
      start_idx <- which(levels_wdays == "Mon")
    } else {
      start_idx <- which(levels_wdays == "Monday")
    }
    ro_seq <- levels_wdays[c(start_idx:7, 1:(start_idx - 1))]
    wdays <- factor(wdays, levels = ro_seq)
  } else {
    wdays <- if_else(wdays == 1, 7, wdays - 1)
  }
  return(wdays)
}
