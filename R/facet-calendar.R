globalVariables("facet_wrap")

#' Lay out panels in a calendar
#'
#' @param date A variable that contains "Date" class.
#' @param format A character string. See `?strptime` for details.
#' @param week_start Day on which week starts following ISO conventions -
#' 1 means Monday, 7 means Sunday (default). You can set `lubridate.week.start` 
#' option to control this parameter globally.
#' @inheritParams ggplot2::facet_wrap
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
facet_calendar <- function(date, nrow = NULL, ncol = NULL, format = "%b %d",
  week_start = getOption("lubridate.week.start", 1), scales = "fixed", 
  shrink = TRUE, dir = "h", strip.position = "top") {

  scales <- match.arg(scales, c("fixed", "free_x", "free_y", "free"))
  dir <- match.arg(dir, c("h", "v"))
  free <- list(
    x = any(scales %in% c("free_x", "free")),
    y = any(scales %in% c("free_y", "free"))
  )

  facet <- facet_wrap(~ .label, nrow = nrow, ncol = ncol, scales = scales, 
    shrink = shrink, strip.position = strip.position)
  facet$params$date <- enexpr(date)
  facet$params$format <- format
  facet$params$week_start <- week_start
  facet$params$free <- free
  facet$params$dir <- dir
  ggproto(NULL, FacetCalendar,
    shrink = shrink,
    params = facet$params
  )
}

#' @rdname facet-calendar
#' @format NULL
#' @usage NULL
#' @export
FacetCalendar <- ggproto("FacetCalendar", FacetWrap,
  compute_layout = function(data, params) {
    eval_date <- eval_tidy(params$date, data = data[[1]])
    date_chr <- as_string(params$date)

    layout <- setup_calendar.monthly(eval_date, dir = params$dir,
      week_start = params$week_start, nrow = params$nrow, ncol = params$ncol)
    n <- NROW(layout)

    layout %>%
      dplyr::mutate(
        !! date_chr := PANEL,
        .label = format.Date(PANEL, format = params$format),
        PANEL = seq_len(n),
        SCALE_X = ifelse(params$free$x, seq_len(n), 1L),
        SCALE_Y = ifelse(params$free$y, seq_len(n), 1L)
      )
  },

  map_data = function(data, layout, params) {
    date_chr <- as_string(params$date)
    dplyr::left_join(data, layout, by = date_chr)
  },

  draw_panels = function(self, panels, layout, x_scales, y_scales, ranges, 
    coord, data, theme, params) {

    canvas <- ggproto_parent(FacetWrap, self)$draw_panels(panels, layout, 
      x_scales, y_scales, ranges, coord, data, theme, params)

    space_x <- theme$panel.spacing.x %||% theme$panel.spacing
    row_spacer <- unit(as.double(space_x) * 2, attr(space_x, "unit"))
    space_y <- theme$panel.spacing.y %||% theme$panel.spacing
    col_spacer <- unit(as.double(space_y) * 2, attr(space_y, "unit"))

    # No idea why 28 and 26 are position in gtable (perhaps magic numbers)
    ncol <- max(layout$MCOL)
    nrow <- max(layout$MROW)
    for (i in seq(28, by = 28, length.out = ncol - 1)) {
      canvas <- gtable::gtable_add_cols(canvas, width = col_spacer, pos = i)
    }
    for (j in seq(26, by = 26, length.out = nrow - 1)) {
      canvas <- gtable::gtable_add_rows(canvas, heights = row_spacer, pos = j)
    }
    canvas
  }
)
