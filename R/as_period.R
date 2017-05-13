#' @title Get weekdays component of datetime
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
