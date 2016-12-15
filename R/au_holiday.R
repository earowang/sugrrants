#' Australia public holidays
#'
#' @param year A vector of integer(s) indicating year(s)
#' @param state A state in Australia. The abbreviation of the states in the upper
#'    letter cases is required. Currently supports "VIC" (i.e. Victoria) only.
#'
#' @return A data frame consisting of \code{holiday} labels and their associated
#'    dates in the year(s).
#'
#' @author Earo Wang
#' @references 
#'    \url{http://www.business.vic.gov.au/victorian-public-holidays-and-daylight-saving/victorian-public-holidays-2014}
#'    \url{http://www.business.vic.gov.au/victorian-public-holidays-and-daylight-saving/victorian-public-holidays}
#' @export
#'
#' @examples
#'    au_holiday(2016, state = "VIC")
#'    au_holiday(2013:2016, state = "VIC")
#'
au_holiday <- function(year, state = "VIC") {
  year_length <- length(year)
  state <- match.arg(state)
  public_holidays <- vector(mode = "list", length = 10)

  i <- 1
  starting <- new_year <- as_date(paste0(year, "-01-01")) # new year's day
  new_year_wday <- wday(new_year)
  # new_year day observed
  new_year <- as_date(if_else(
    new_year_wday == 1, # Sunday
    new_year + days(1), # shift to next Monday
    new_year
  ))
  new_year <- as_date(if_else(
    new_year_wday == 7, # Saturday
    new_year + days(2), # shift to next Monday
    new_year
  ))
  public_holidays[[i]] <- new_year

  i <- i + 1
  australia <- as_date(paste0(year, "-01-26")) # australia day
  australia_wday <- wday(australia)
  # australia day observed
  australia <- as_date(if_else(
    australia_wday == 1, # Sunday
    australia + days(1), # shift to next Monday
    australia
  ))
  australia <- as_date(if_else(
    australia_wday == 7, # Saturday
    australia + days(2), # shift to next Monday
    australia))
  public_holidays[[i]] <- australia

  i <- i + 1
  lubridate::month(starting) <- 3 # switch to March
  starting_wday <- wday(starting)
  diff_mon <- 2 - starting_wday
  # Find the first Monday in March
  march_1mon <- starting + if_else(diff_mon >= 0, diff_mon, diff_mon + 7)
  # Find the second Monday in March (labour day)
  labour <- march_1mon + weeks(1) # VIC and Tas
  public_holidays[[i]] <- labour

  i <- i + 1
  good_friday <- as_date(GoodFriday(year))
  public_holidays[[i]] <- good_friday
  i <- i + 1
  easter_sun <- as_date(Easter(year))
  public_holidays[[i]] <- easter_sun
  i <- i + 1
  easter_mon <- as_date(Easter(year)) + days(1)
  public_holidays[[i]] <- easter_mon

  i <- i + 1
  anzac <- as_date(paste0(year, "-04-25")) # regardless of weekday
  public_holidays[[i]] <- anzac

  i <- i + 1
  lubridate::month(starting) <- 6 # switch to June
  starting_wday <- wday(starting)
  diff_mon <- 2 - starting_wday
  # Find the first Monday in June
  june_1mon <- starting + if_else(diff_mon >= 0, diff_mon, diff_mon + 7)
  # Find the second Monday in June (Queen's birthday)
  queens <- june_1mon + weeks(1) # except QLD, WA
  public_holidays[[i]] <- queens

  # AFL grand final day has become Public holiday in VIC since 2015
  # if (any(year == 2015)) {
  #   grand_final <- as_date("2015-10-02")
  # }
  # if (any(year == 2016)) {
  #   grand_final <- as_date("2016-09-30")
  # }

  i <- i + 1
  lubridate::month(starting) <- 11 # switch to Nov
  starting_wday <- wday(starting)
  diff_tue <- 3 - starting_wday
  # Find the first Tuesday in Nov (Melbourne cup)
  melb_cup <- starting + if_else(diff_tue >= 0, diff_tue, diff_tue + 7)
  public_holidays[[i]] <- melb_cup

  i <- i + 1
  christmas <- as_date(paste0(year, "-12-25"))
  christmas_wday <- wday(christmas)
  # Substitute for Sat/Sun 25 Dec
  christmas <- as_date(if_else(
    christmas_wday == 1, # Sunday
    christmas + days(2), # shift to next Tuesday
    christmas
  ))
  christmas <- as_date(if_else(
    christmas_wday == 7, # Saturday
    christmas + days(2), # shift to next Monday
    christmas
  ))
  public_holidays[[i]] <- christmas

  i <- i + 1
  boxing <- as_date(paste0(year, "-12-26"))
  boxing_wday <- wday(boxing)
  # Substitute for Sat/Sun 26 Dec
  boxing <- as_date(if_else(
    boxing_wday == 1, # Sunday
    boxing + days(1), # shift to next Monday
    boxing
  ))
  boxing <- as_date(if_else(
    boxing_wday == 7, # Saturday
    boxing + days(2), # shift to next Monday
    boxing
  ))
  public_holidays[[i]] <- boxing
  public_holidays <- as_date(unlist(public_holidays))
  sort_idx <- order(public_holidays)

  # Holiday labels
  hdays_labels <- c(
    "New Year's Day", "Australia Day", "Labour Day",
    "Good Friday", "Easter Sunday", "Easter Monday",
    "ANZAC Day", "Queen's Birthday", "Melbourne Cup",
    "Christmas Day", "Boxing Day"
  )
  hdays_df <- tibble(
    holiday = rep(hdays_labels, each = year_length),
    date = public_holidays
  )
  hdays_df <- hdays_df[sort_idx, ]

  return(hdays_df)
}
