# season <- function(x, hemisphere = c("N", "S"), 
#                    definition = c("meterological", "astronomical", 
#                                   "midsolstice")) {
#   hemisphere <- match.arg(hemisphere)
#   definition <- match.arg(definition)
#   month_x <- month(x)
#   if (hemisphere == "N") {
#     season_x <- case_when(
#       month_x %in% 3:5 ~ "Spring",
#       month_x %in% 6:8 ~ "Summer",
#       month_x %in% 9:11 ~ "Autumn",
#       month_x %in% c(12, 1:2) ~ "Winter"
#     )
#   } else {
#     season_x <- case_when(
#       month_x %in% 3:5 ~ "Autumn",
#       month_x %in% 6:8 ~ "Winter",
#       month_x %in% 9:11 ~ "Spring",
#       month_x %in% c(12, 1:2) ~ "Summer"
#     )
#   }
#   return(season_x)
# }
#
# as_season <- function(x, hemisphere = "N", definition = "meterological",
#                       date_format = TRUE) {
#   hemisphere <- match.arg(hemisphere, c("N", "S"))
#   definition <- match.arg(
#     definition, c("meterological", "astronomical", "midsolstice")
#   )
#
#   year_x <- year(x)
#   season_x <- season(x, hemisphere = hemisphere, definition = definition)
#   if (date_format) {
#     if (hemisphere == "N") {
#       season_date <- case_when(
#         season_x == "Spring" ~ "03-01",
#         season_x == "Summer" ~ "06-01",
#         season_x == "Autumn" ~ "09-01",
#         season_x == "Winter" ~ "12-01"
#       )
#     } else {
#       season_date <- case_when(
#         season_x == "Autumn" ~ "03-01",
#         season_x == "Winter" ~ "06-01",
#         season_x == "Spring" ~ "09-01",
#         season_x == "Summer" ~ "12-01"
#       )
#     }
#     output <- ymd(paste(year_x, season_date, sep = "-"))
#   } else {
#     output <- paste(year_x, season_x, sep = "-")
#   }
#   return(output)
# }

#' @name as_month
#' @aliases as_week
#'
#' @title Extract the calendar period components from date-times
#'
#' @description Extract the higher-level calendar period components from date-times
#'
#' @param x Date-times.
#'
#' @return The period of `x` as date format or character string. It always
#'    returns the first day of the month (`as_month`) and the first day of
#'    the week (`as_week`).
#'
#' @author Earo Wang
#' 
#' @rdname as_month
#' 
#' @export
#'
#' @examples
#'    # x is a vector of class POSIXct
#'    x <- as.POSIXct(c("2014-01-31", "2015-07-31", "2016-10-31"))
#'    as_month(x)
#'    as_week(x)
#'
#'    # an example of a data frame
#'    # compute average pedestrian counts for every week
#'    library(dplyr)
#'    pedestrian %>% 
#'      filter(Sensor_ID == 13) %>% 
#'      mutate(Week = as_week(Date_Time)) %>% 
#'      group_by(Week) %>% 
#'      summarise(Average = mean(Hourly_Counts, na.rm = TRUE))
#'    
# apply to monthly data
# x takes datetime stamp and return the year-month-first of day
as_month <- function(x) {
  year_x <- year(x)
  month_x <- formatC(month(x), width = 2, flag = "0")
  output <- ymd(paste0(year_x, month_x, "01"))
  return(output)
}

#' @rdname as_month
#' @export
# apply to weekly data
# x takes datetime stamp and return the first date of the week
as_week <- function(x) {
  wday_x <- wday(x, label = FALSE, abbr = FALSE)
  diff_wday <- if_else(wday_x == 1, -6, 2 - wday_x) # find first date in the week
  output <- as_date(x + days(diff_wday))
  return(output)
}

#' @title Get weekdays component of datetimea
#'
#' @description A simple wrapper of `lubridate::wday()`, but starts with
#'    Monday instead of Sunday.
#'
#' @param x Date-times.
#' @param label Logical. If TRUE, it displays the day of the week as an ordered
#'    character strings; otherwise, an integer returns.
#' @param abbr Logical. If TRUE, it returns an abbreviated version the label.
#'
#' @return The day of the week as a number (Monday is 1) or an ordered factor
#'    (Monday is first).
#'
#' @seealso [lubridate::wday]
#'
#' @author Earo Wang
#' 
#' @export
#'
#' @examples
#'    # x is a vector of class POSIXct
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
