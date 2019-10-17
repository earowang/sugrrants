globalVariables(c(
  ".group_id", ".gx", ".gy", ".xmajor_min", ".xminor_max", ".xminor_min",
  ".ymajor_max", ".ymajor_min", ".yminor_max", ".yminor_min", "COL", "PANEL",
  "MCOL", "MPANEL", "MROW", "ROW", "date_eval", "label", "margin", "radius",
  "theta", "x", "y", ".day", ".calendar_tbl", ".x", ".y", ".", ".ymax", ".ymin",
  ".cx", ".cy", "label_arg"
))

#' Rearrange a temporal data frame to a calendar-based data format using linear algebra
#'
#' Temporal data of daily intervals or higher frequency levels can be organised
#' into a calendar-based format, which is useful for visually presenting
#' calendar-related activities or multiple seasonality (such as time of day,
#' day of week, day of month). The function only returns a rearranged data frame,
#' and `ggplot2` takes care of the plotting afterwards. It allows more
#' flexibility for users to visualise the data in various ways.
#'
#' @param data A data frame or a grouped data frame including a `Date` variable.
#' @param x A bare (or unquoted) variable mapping to x axis, for example time of
#' day. If integer 1 is specified, it simply returns calendar grids on x
#' without transformation.
#' @param y A bare (or unquoted) variable or more mapping to y axis. More than
#' one variable need putting to `vars()`. If integer 1 is specified, it returns
#' calendar grids on y without transformation.
#' @param date A `Date` variable mapping to dates in the calendar.
#' @param calendar Type of calendar. (1) "monthly" calendar (the default) organises
#' the `data` to a common format comprised of day of week in the column and
#' week of month in the row. A monthly calendar is set up as a 5 by 7 layout
#' matrix. Each month could extend over six weeks but in these months is to
#' wrap the last few days up to the top row of the block. (2) "weekly"
#' calendar consists of day of week and week of year. (3) "daily" calendar
#' refers to day of month and month of year.
#' @param dir Direction of calendar: "h" for horizontal (the default) or "v" for
#' vertical.
#' @param week_start Day on which week starts following ISO conventions -
#' 1 means Monday (default), 7 means Sunday. You can set `lubridate.week.start`
#' option to control this parameter globally.
#' @param nrow,ncol Number of rows and columns defined for "monthly" calendar
#' layout. If `NULL`, it computes a sensible layout.
#' @param polar FALSE (the default) for Cartesian or TRUE for polar coordinates.
#' @param scale "fixed" (the default) for fixed scale. "free" for scaling
#' conditional on each daily cell, "free_wday" for scaling on weekdays,
#' "free_mday" for scaling on day of month.
#' @param width,height Numerics between 0 and 1 to specify the width/height for
#' each glyph.
#' @param margin Numerics of length two between 0 and 1 to specify the horizontal
#' and vertical margins between month panels.
#'
#' @return A data frame or a dplyr::tibble with newly added columns of `.x`, `.y`. `.x`
#' and `.y` together give new coordinates computed for different types of
#' calendars. `date` groups the same dates in a chronological order, which is
#' useful for `geom_line` or `geom_path`. The basic use is `ggplot(aes(x = .x,
#' y = .y, group = date)) + geom_*`. The variable names `.x` and `.y` reflect
#' the actual `x` and `y` with a prefix `.`.
#'
#' @details The calendar-based graphic can be considered as small multiples
#' of sub-series arranged into many daily cells. For every multiple (or
#' facet), it requires the `x` variable mapped to be time of day and `y` to
#' value. New `x` and `y` are computed and named with a `.` prefixed to variable
#' according to `x` and `y` respectively, and get ready for `ggplot2` aesthetic
#' mappings. In conjunction with `group_by()`, it allows the grouped variable
#' to have their individual scales. For more details, see `vignette("frame-calendar",
#' package = "sugrrants")`
#'
#' @rdname frame-calendar
#' @seealso [facet_calendar] for a fully-fledged faceting calendar with formal
#' labels and axes.
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#' # compute the calendar layout for the data frame
#' calendar_df <- pedestrian %>%
#'   filter(Sensor_ID == 13, Year == 2016) %>%
#'   frame_calendar(x = Time, y = Hourly_Counts, date = Date, nrow = 4)
#'
#' # ggplot
#' p1 <- calendar_df %>%
#'   ggplot(aes(x = .Time, y = .Hourly_Counts, group = Date)) +
#'   geom_line()
#' prettify(p1, size = 3, label.padding = unit(0.15, "lines"))
#'
#' # use in conjunction with group_by()
#' grped_calendar <- pedestrian %>%
#'   filter(Year == "2017", Month == "March") %>%
#'   group_by(Sensor_Name) %>%
#'   frame_calendar(x = Time, y = Hourly_Counts, date = Date, week_start = 7)
#'
#' p2 <- grped_calendar %>%
#'   ggplot(aes(x = .Time, y = .Hourly_Counts, group = Date)) +
#'   geom_line() +
#'   facet_wrap(~ Sensor_Name, nrow = 2)
#' prettify(p2)
#' \dontrun{
#' # allow for different languages
#' # below gives simplied Chinese labels with STKaiti font family,
#' # assuming this font installed in user's local system
#' prettify(p2, locale = "zh", family = "STKaiti")
#'
#' # plotly example
#' if (!requireNamespace("plotly", quietly = TRUE)) {
#'   stop("Please install the 'plotly' package to run these following examples.")
#' }
#' library(plotly)
#' pp <- calendar_df %>%
#'   group_by(Date) %>%
#'   plot_ly(x = ~ .Time, y = ~ .Hourly_Counts) %>%
#'   add_lines(text = ~ paste("Count: ", Hourly_Counts, "<br> Time: ", Time))
#' prettify(pp)
#' }
#'
#' @export
frame_calendar <- function(
  data, x, y, date, calendar = "monthly", dir = "h",
  week_start = getOption("lubridate.week.start", 1),
  nrow = NULL, ncol = NULL, polar = FALSE, scale = "fixed",
  width = 0.95, height = 0.95, margin = NULL, ...
) {
  calendar <- match.arg(calendar, c("monthly", "weekly", "daily"))
  dir <- match.arg(dir, c("h", "v"))
  scale <- match.arg(scale, c("fixed", "free", "free_wday", "free_mday"))

  UseMethod("frame_calendar")
}

#' @export
frame_calendar.tbl_ts <- function(data, x, y, date, ...) {
  x <- enquo(x)
  y <- enquo(y)
  date <- enquo(date)

  out <- frame_calendar.default(data, x = !! x, y = !! y, date = !! date, ...)
  if (tsibble::is_grouped_ts(data)) {
    out <- out %>%
      group_by(!!! groups(data))
  }
  if (utils::packageVersion("tsibble") < "0.8.0") {
    abort("tsibble (>= 0.8.0) is required.")
  }
  out <- tsibble::build_tsibble(
    out, key = !! tsibble::key_vars(data), index = !! tsibble::index(data),
    index2 = !! tsibble::index2(data), interval = tsibble::interval(data),
    ordered = tsibble::is_ordered(data), validate = FALSE
  )
  class(out) <- c("tbl_cal", class(out))
  out
}

#' @export
frame_calendar.grouped_ts <- frame_calendar.tbl_ts

#' @export
frame_calendar.grouped_df <- function(data, x, y, date, ...) {
  x <- enquo(x)
  y <- enquo(y)
  date <- enquo(date)
  grps <- dplyr::groups(data)

  out <- frame_calendar.default(data, x = !! x, y = !! y, date = !! date, ...)
  out <- group_by(out, !!! grps)
  class(out) <- c("tbl_cal", class(out))
  out

}

#' @export
frame_calendar.default <- function(
  data, x, y, date, calendar = "monthly", dir = "h",
  week_start = getOption("lubridate.week.start", 1),
  nrow = NULL, ncol = NULL, polar = FALSE, scale = "fixed",
  width = 0.95, height = 0.95, margin = NULL, ...
) {
  if (NROW(data) == 0L) {
    abort("Facet calendar must contain observations.")
  }
  if (identical(between(width, 0, 1) && between(height, 0, 1), FALSE)) {
    abort("`width`/`height` must be between 0 and 1.")
  }

  # data <- arrange(data, !!date) # I don't think I need to change row order
  x <- enquo(x)
  y <- enquo(y)
  date <- enquo(date)
  if (any(!map_lgl(list(x, y, date), function(x) quo_is_symbolic(x) || is_identity(x)))) {
    abort("Arguments `x`, `y`, and `Date` must be unquoted variables not characters.")
  }

  .x <- paste0(".", quo_name(x))
  y_expr <- quo_get_expr(y)
  if (quo_is_call(y)) {
    fn <- call_name(y)
    if (fn != "vars") {
      abort("Multiple variables must be used with `vars()` for `y`.")
    }
    y <- quos(!!! call_args(y))
  } else {
    y <- quos(!! y_expr)
  }
  .y <- paste0(".", map_chr(y, quo_name))
  .date <- quo_name(date)
  cls <- class(data)
  old_cn <- names(data)

  is_grped <- dplyr::is_grouped_df(data)
  grp_vars <- dplyr::groups(data)

  # as some variables have been created for the computation,
  # if `data` has those variables, they're likely to be overwritten.
  # an error of conflicts is thrown away.
  int_vars <- c(".gx", ".gy", ".cx", ".cy", ".ymax", ".ymin")
  if (is_identity(x)) int_vars <- c(int_vars, ".x")
  if (is_identity(y)) int_vars <- c(int_vars, ".y")

  if (polar) int_vars <- c(int_vars, "theta", "radius")
  if (scale %in% c("free_wday", "free_mday")) int_vars <- c(int_vars, ".day")
  check_vars <- int_vars %in% old_cn
  if (any(check_vars)) {
    str_vars <- int_vars[check_vars]
    abort(sprintf(
      "Columns %s must be renamed to proceed.",
      paste(str_vars, collapse = ", ")
    ))
  }

  date_eval <- sort(eval_tidy(date, data = data))
  if (!("Date" %in% class(date_eval))) {
    abort(sprintf("`date` (`%s`) must be a `Date` class."), .date)
  }

  if (calendar != "monthly") {
    if (!is.null(nrow) || !is.null(ncol)) {
      inform("Argument `nrow`/`ncol` only works for the monthly calendar.")
    }
  }

  class(date_eval) <- c(calendar, class(date_eval))
  cal_layout <- setup_calendar(x = date_eval, dir = dir, week_start = week_start,
    nrow = nrow, ncol = ncol, ...)

  # Assign grids to the panels
  grids <- assign_grids(
    max(cal_layout$ROW), max(cal_layout$COL),
    width = width, height = height, polar = polar
  )
  cal_grids <- cal_layout %>%
    left_join(grids, by = c("COL", "ROW"))

  # ideally should use left_join as keeping the colnames in the supplied order
  # but quo_name() doesn't support LHS
  data <- cal_grids %>%
    right_join(data, by = c("PANEL" = quo_name(date))) %>%
    dplyr::mutate(!! .date := PANEL)

  # Define a small multiple width and height
  width <- resolution(data$.gx, zero = FALSE) * width
  height <- resolution(data$.gy, zero = FALSE) * height
  if (is.null(margin)) {
    margin <- c(width, height) # Month by month margin
  } else if (has_length(margin, 1)) {
    margin <- rep(margin, 2)
  }

  if (calendar == "monthly") {
    data <- data %>%
      dplyr::group_by(MPANEL) %>%
      dplyr::mutate(
        .gx = .gx + MCOL * margin[1],
        .gy = .gy - MROW * margin[2],
        .cx = .cx + MCOL * margin[1],
        .cy = .cy - MROW * margin[2]
      )
  }

  data <- ungroup(data) # is.null(scale)
  if (scale == "free") {
    data <- dplyr::group_by(data, ROW, COL)
  } else if (scale == "free_wday") {
    data <- data %>%
      dplyr::mutate(.day = wday(!! date)) %>%
      dplyr::group_by(.day)
  } else if (scale == "free_mday") {
    data <- data %>%
      dplyr::mutate(.day = mday(!! date)) %>%
      dplyr::group_by(.day)
  }

  if (is_grped) {
    data <- data %>%
      group_by(!!! grp_vars)
  }

  data <- data %>%
    dplyr::mutate(
      .ymax = max_na(!!! y),
      .ymin = min_na(!!! y)
    )
  if (polar) { # polar only support one y
    if (length(y) > 1) {
      message("Only the first `y` variable is used.")
    }
    .y <- .y[[1]]
    data <- data %>%
      dplyr::mutate(
        theta = 2 * pi * normalise(as.numeric(!! x),
          xmax = max_na(as.numeric(!! x))),
        radius = normalise(as.numeric(!!! y), xmax = max_na(as.numeric(!!! y))),
        !! .x := .cx + width / 2 * radius * sin(theta),
        !! .y := .cy + height / 2 * radius * cos(theta)
      ) %>%
      dplyr::select(-c(theta, radius))
  } else {
    fn <- function(x, ymax, ymin) { # temporal function for mutate at
      normalise(x, xmax = max_na(ymax), xmin = min_na(ymin)) * height
    }
    if (is_identity(x)) {
      data <- dplyr::mutate(data, .x = .cx)
    } else {
      data <- data %>%
        dplyr::mutate(
          !! .x := .cx + normalise(as.numeric(!! x),
            xmax = max_na(as.numeric(!! x))) * width
        )
    }
    if (is_identity(y)) {
      data <- dplyr::mutate(data, .y = .cy)
    } else {
      data <- data %>%
        dplyr::mutate_at(
          .vars = vars(!!! y),
          .funs = list(zzz = ~ .cy + fn(., .ymax, .ymin))
        )
    }
  }
  # generate breaks and labels for prettify()
  class(cal_grids) <- c(calendar, class(cal_grids))
  data_ref <- gen_reference(
    cal_grids, margin, calendar = calendar, week_start = week_start, dir = dir
  )

  data <- ungroup(data) %>%
    dplyr::select(-c(ROW:.cy, .ymax, .ymin))
  if (scale %in% c("free_wday", "free_mday")) {
    data <- dplyr::select(data, -.day) # remove .day variable
  }

  # rename y's variables
  y_idx <- ends_with("zzz", vars = names(data))
  names(data)[y_idx] <- .y

  # use original column ordering
  data <- data %>%
    dplyr::select(old_cn, .x, .y)

  new_calendar(
    data,
    breaks = data_ref$breaks,
    minor_breaks = data_ref$minor_breaks,
    label = data_ref$label,
    text = data_ref$text,
    text2 = data_ref$text2,
    dir = dir,
    margin = margin,
    calendar = calendar
  )
}

## calendar functions -------------------
# Assign grids from 0 to 1
assign_grids <- function(ROW, COL, width, height, polar = FALSE) {
  col_grids <- seq(1, 0, length.out = ROW)
  row_grids <- seq(0, 1, length.out = COL)
  grids <- expand.grid2(.gx = row_grids, .gy = col_grids)
  combs <- expand.grid2(COL = seq_len(COL), ROW = seq_len(ROW))
  out <- cbind(combs, grids)
  min_x <- min_diff(row_grids)
  min_y <- min_diff(col_grids)
  if (polar) {
    out$.cx <- out$.gx + min_x / 2
    out$.cy <- out$.gy + min_y / 2
  } else {
    out$.cx <- out$.gx + (min_x - (width / (COL - 1))) / 2
    out$.cy <- out$.gy
  }
  out
}

# Compute grid lines and text labels for frame_calendar()
gen_reference <- function(grids, dir = "h", ...) {
  dir <- match.arg(dir, c("h", "v"))
  UseMethod("gen_reference")
}

gen_reference.daily <- function(grids, dir = "h", ...) {
  # day breaks
  minor_breaks <- gen_day_breaks(grids)
  min_width <- min_diff(minor_breaks$x)
  min_height <- min_diff(minor_breaks$y)

  # Prepare for the string texts
  date <- grids$PANEL
  yrs <- year(date)
  nyears <- unique.default(yrs)
  month_idx <- month(date)
  unique_idx <- unique.default(paste(yrs, month_idx))
  unique_labels <- as.integer(substring(unique_idx, first = 6))

  if (dir == "h") {
    # Month text positioned at the right of each month panel (dir == "h")
    mtext <- dplyr::tibble(
      x = max(minor_breaks$x) + min_width,
      y = minor_breaks$y + min_height / 2
    )
    # Weekday text at the bottom
    dtext <- dplyr::tibble(
      x = minor_breaks$x + min_width / 2,
      y = min(minor_breaks$y)
    )
  } else {
    # Month text positioned at the top of each month panel (dir == "v")
    mtext <- dplyr::tibble(
      x = minor_breaks$x,
      y = max(minor_breaks$y) + min_height
    )
    dtext <- dplyr::tibble(
      x = min(minor_breaks$x),
      y = minor_breaks$y + min_height / 2
    )
  }
  mtext$mon <- unique_labels
  dtext$label <- seq_len(max(mday(date)))

  list(breaks = NULL, minor_breaks = minor_breaks, label = mtext, text = dtext)
}

gen_reference.weekly <- function(grids, dir = "h", ...) {
  # day breaks
  minor_breaks <- gen_day_breaks(grids)
  min_width <- min_diff(minor_breaks$x)
  min_height <- min_diff(minor_breaks$y)

  # Prepare for the string texts
  date <- grids$PANEL
  yrs <- year(date)
  nyears <- unique.default(yrs)
  week_labels <- paste(yrs, isoweek(date), sep = "-")
  unique_labels <- formatC( # make week index to be fixed-width 2 digits
    as.integer(substring(unique(week_labels), first = 6)),
    width = 2, flag = "0"
  )
  unique_labels <- rle(unique_labels)$values # rm duplicates in run length

  if (dir == "h") {
    # Week index text positioned at the right of each week panel (dir == "h")
    mtext <- dplyr::tibble(
      x = max(minor_breaks$x) + min_width,
      y = minor_breaks$y
    )
    # Weekday text at the bottom
    dtext <- dplyr::tibble(
      x = minor_breaks$x + min_width / 2,
      y = min(minor_breaks$y)
    )
  } else {
    # Week index text positioned at the top of each week panel (dir == "v")
    mtext <- dplyr::tibble(
      x = minor_breaks$x,
      y = max(minor_breaks$y) + min_height
    )
    dtext <- dplyr::tibble(
      x = min(minor_breaks$x),
      y = minor_breaks$y + min_height / 2
    )
  }
  mtext$label <- unique_labels
  dtext$day <- gen_wday_index(week_start = 1)

  list(breaks = NULL, minor_breaks = minor_breaks, label = mtext, text = dtext)
}

gen_reference.monthly <- function(
  grids, margin, dir = "h", week_start = 1, ...
) {
  # Month breaks
  grids <- arrange(grids, PANEL)
  grids <- grids %>%
    group_by(MPANEL) %>%
    mutate(
      .gx = .gx + MCOL * margin[1],
      .gy = .gy - MROW * margin[2]
    )
  xbreaks_df <- grids %>%
    group_by(MCOL) %>%
    summarise(
      .xmajor_min = min(.gx),
      .xmajor_max = max(.gx)
    )
  ybreaks_df <- grids %>%
    group_by(MROW) %>%
    summarise(
      .ymajor_min = min(.gy),
      .ymajor_max = max(.gy)
    )

  # day breaks
  minor_breaks <- gen_day_breaks(grids)
  min_width <- min_diff(minor_breaks$x)
  min_height <- min_diff(minor_breaks$y)

  # month breaks update
  xbreaks <- c(xbreaks_df$.xmajor_min, xbreaks_df$.xmajor_max + min_width)
  ybreaks <- c(ybreaks_df$.ymajor_min, ybreaks_df$.ymajor_max + min_height)
  breaks <- list(x = xbreaks, y = ybreaks)

  # Prepare for the string texts
  date <- grids$PANEL
  yrs <- year(date)
  nyears <- unique.default(yrs)
  month_idx <- month(date)
  unique_idx <- unique.default(paste(yrs, month_idx))
  unique_labels <- as.integer(substring(unique_idx, first = 6))

  # Month label positioned at the top left of each month panel
  xtext <- sort(xbreaks_df$.xmajor_min)
  ytext <- sort(ybreaks_df$.ymajor_max + min_height, decreasing = TRUE)
  mtext <- dplyr::as_tibble(expand.grid2(x = xtext, y = ytext))
  mtext <- mtext[seq_along(unique_idx), , drop = FALSE]
  mtext$mon <- unique_labels
  mtext$year <- paste(substring(unique_idx, first = 1, last = 4), "")
  mtext$year[duplicated(mtext$year)] <- "" # make year appear in the first month

  # Weekday text
  if (dir == "h") {
    dtext <- dplyr::tibble(
      x = minor_breaks$x + min_width / 2,
      y = min(minor_breaks$y)
    )
  } else {
    dtext <- dplyr::tibble(
      x = min(minor_breaks$x),
      y = minor_breaks$y + min_height / 2
    )
  }
  dtext$day <- gen_wday_index(week_start = week_start)

  # Day of month text
  mday_text <- dplyr::tibble(
    x = grids$.gx,
    y = grids$.gy + min_height,
    label = mday(grids$PANEL)
  )

  list(
    breaks = breaks, minor_breaks = minor_breaks,
    label = mtext, text = dtext, text2 = mday_text
  )
}

new_calendar <- function(x, ...) {
  # Can't use structure() here because it breaks the row.names attribute
  attribs <- list(...)

  attributes(x)[names(attribs)] <- attribs
  attr(x, "row.names") <- .set_row_names(NROW(x))
  class(x) <- c("tbl_cal", class(x))
  x
}
