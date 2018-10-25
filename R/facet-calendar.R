globalVariables("facet_wrap")

#' Lay out panels in a calendar
#'
#' @param date A variable that contains "Date" class.
#' @inheritParams ggplot2::facet_wrap
#' @param calendar.label Either "wday" or "mday".
#'
#' @rdname facet-calendar
#' @export
#' @examples
#' fs <- pedestrian %>%
#'   dplyr::filter(Sensor_Name == "Flagstaff Station", Date < as.Date("2016-05-01"))
#' 
#' fs %>%
#'   ggplot(aes(x = Time, y = Hourly_Counts, group = Date)) +
#'   geom_line() +
#'   facet_calendar(date = Date, nrow = 2)
facet_calendar <- function(date, nrow = NULL, ncol = NULL, calendar.label = "mday",
  scales = "fixed", shrink = TRUE, strip.position = "top") {
  calendar.label <- match.arg(calendar.label, c("wday", "mday"))

  if (calendar.label == "mday") {
    facet <- ggplot2::facet_wrap(~ .month + .mday, nrow = nrow, ncol = ncol,
      scales = scales, shrink = shrink, strip.position = strip.position)
    facet$params$date <- enexpr(date)
    ggproto(NULL, FacetCalendarMday,
      shrink = shrink,
      params = facet$params
    )
  }
}

#' @rdname facet-calendar
#' @format NULL
#' @usage NULL
#' @export
FacetCalendarMday <- ggproto("FacetCalendarMday", FacetWrap,
  compute_layout = function(data, params) {
    eval_date <- eval_tidy(params$date, data = data[[1]])
    date_chr <- as_string(params$date)

    layout <- setup_calendar.monthly(eval_date, 
      nrow = params$nrow, ncol = params$ncol)

    layout %>%
      dplyr::mutate(
        !! date_chr := PANEL,
        .month = lubridate::month(PANEL, label = TRUE),
        .mday = lubridate::mday(PANEL),
        PANEL = dplyr::row_number(),
        SCALE_X = 1L,
        SCALE_Y = 1L
      )
  },

  map_data = function(data, layout, params) {
    date_chr <- as_string(params$date)
    data %>%
      dplyr::left_join(layout, by = date_chr)
  }
)
