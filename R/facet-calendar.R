globalVariables("facet_wrap")

#' Lay out panels in a calendar format
#'
#' @param date A variable that contains dates or an expression that generates 
#' dates will be mapped in the calendar.
#' @param format A character string, such as `%Y-%b-%d` and `%a (%d)`, formatting
#' the display of facet strips. See `?strptime` for details.
#' @param week_start Day on which week starts following ISO conventions -
#' 1 means Monday, 7 means Sunday (default). You can set `lubridate.week.start` 
#' option to control this parameter globally.
#' @param nrow,ncol Number of rows and columns defined for "monthly" calendar
#' layout. If `NULL`, it computes a sensible layout.
#' @param dir Direction of calendar: "h" for horizontal (the default) or "v" for
#' vertical.
#' @inheritParams ggplot2::facet_wrap
#' @importFrom gtable gtable_add_cols gtable_add_rows
#'
#' @details A monthly calendar is set up as a 5 by 7 layout matrix. Each month could 
#' extend over six weeks but in these months is to wrap the last few days up 
#' to the top row of the block.
#'
#' @rdname facet-calendar
#' @seealso [frame_calendar] for a compact calendar display, by quickly transforming
#' the data.
#' @export
#' @examples
#' \donttest{
#' fs <- pedestrian %>%
#'   dplyr::filter(Date < as.Date("2016-05-01"))
#'
#' fs %>%
#'   ggplot(aes(x = Time, y = Hourly_Counts)) +
#'   geom_line(aes(colour = Sensor_Name)) +
#'   facet_calendar(~ Date, nrow = 2) + # or ~ as.Date(Date_Time)
#'   theme(legend.position = "bottom")
#' }
facet_calendar <- function(date, format = "%b %d",
  week_start = getOption("lubridate.week.start", 1), 
  nrow = NULL, ncol = NULL, scales = "fixed", shrink = TRUE, dir = "h", 
  labeller = "label_value", strip.position = "top") {

  scales <- match.arg(scales, c("fixed", "free_x", "free_y", "free"))
  dir <- match.arg(dir, c("h", "v"))
  free <- list(
    x = any(scales %in% c("free_x", "free")),
    y = any(scales %in% c("free_y", "free"))
  )

  facet <- facet_wrap(~ .label, nrow = nrow, ncol = ncol, scales = scales, 
    shrink = shrink, strip.position = strip.position)
  facet$params$date <- as_facet_date(enexpr(date))
  facet$params$format <- format
  facet$params$week_start <- week_start
  facet$params$free <- free
  facet$params$dir <- dir
  facet$params$labeller <- labeller
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
    date_chr <- expr_text(params$date)

    if (!(inherits(eval_date, "Date"))) {
      abort(sprintf(
        "Argument `date` must be class 'Date', not '%s'.", 
        class(eval_date)[[1]]
      ))
    }

    if (NROW(data[[1]]) == 0L) {
      abort("Facet calendar must have at least one date.")
    } 
    layout <- setup_calendar.monthly(eval_date, dir = params$dir,
      week_start = params$week_start, nrow = params$nrow, ncol = params$ncol)
    n <- NROW(layout)

    layout %>%
      dplyr::mutate(
        !! date_chr := PANEL,
        .label = format.Date(PANEL, format = params$format),
        PANEL = factor(seq_len(n), levels = seq_len(n)),
        SCALE_X = ifelse(params$free$x, seq_len(n), 1L),
        SCALE_Y = ifelse(params$free$y, seq_len(n), 1L)
      )
  },

  map_data = function(data, layout, params) {
    date_chr <- expr_text(params$date)
    if (is_call(params$date)) {
      data <- dplyr::mutate(data, !! date_chr := !! params$date)
    }
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
    if (params$dir == "h") {
      for (i in seq(28, by = 28, length.out = ncol - 1)) {
        canvas <- gtable_add_cols(canvas, width = col_spacer, pos = i)
      }
      for (j in seq(26, by = 26, length.out = nrow - 1)) {
        canvas <- gtable_add_rows(canvas, heights = row_spacer, pos = j)
      }
    } else {
      for (i in seq(21, by = 21, length.out = ncol - 1)) {
        canvas <- gtable_add_cols(canvas, width = col_spacer, pos = i)
      }
      for (j in seq(36, by = 36, length.out = nrow - 1)) {
        canvas <- gtable_add_rows(canvas, heights = row_spacer, pos = j)
      }
    }
    canvas
  }
)

as_facet_date <- function(x) {
  if (is_string(x)) {
    x <- parse_expr(x)
  } 
  if (is_formula(x)) {
    x <- f_rhs(x)
  }
  x
}
