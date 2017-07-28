globalVariables(c(
  ".group_id", ".gx", ".gy", ".xmajor_min", ".xminor_max", ".xminor_min", 
  ".ymajor_max", ".ymajor_min", ".yminor_max", ".yminor_min", "COL", "PANEL",
  "MCOL", "MPANEL", "MROW", "ROW", "date_eval", "label", "margins", "radius", 
  "theta", "x", "y", ".day", ".calendar_tbl", ".x", ".y", ".", ".ymax", ".ymin",
  ".cx", ".cy"
))

#' @title Rearrange a temporal data frame to a calendar-based data format using 
#'    linear algebra
#'
#' @description Temporal data of daily intervals or higher frequency levels can 
#'    be organised into a calendar-based format, which is useful for visually
#'    presenting calendar-related activities or multiple seasonality (such as 
#'    time of day, day of week, day of month). The function only returns a
#'    rearranged data frame, and `ggplot2` takes care of the plotting afterwards. 
#'    It allows more flexibility for users to visualise the data in various ways.
#'
#' @param data A data frame or a grouped data frame including a `Date` variable.
#' @param x A bare (or unquoted) variable mapping to x axis, for example time of 
#'    day. If integer 1 is specified, it simply returns calendar grids on x 
#'    without transformation.
#' @param y A bare (or unquoted) variable or more mapping to y axis. More than 
#'    one variable need putting to `vars()`. If integer 1 is specified, it returns
#'    calendar grids on y without transformation.
#' @param date A `Date` variable mapping to dates in the calendar.
#' @param calendar Type of calendar. "monthly" calendar (the default) organises
#'    the `data` to a common format comprised of day of week in the column and
#'    week of month in the row. "weekly" calendar consists of day of week and
#'    week of year. "daily" calendar refers to day of month and month of year.
#' @param dir Direction of calendar: "h" for horizontal (the default) or "v" for 
#'    vertical.
#' @param sunday FALSE (the default) indicating to starting with Monday in a
#'    week, or TRUE for Sunday, when `calendar = "monthly"`.
#' @param nrow,ncol Number of rows and columns defined for "monthly" calendar 
#'    layout. If `NULL`, it computes a sensible layout.
#' @param polar FALSE (the default) for Cartesian or TRUE for polar coordinates.
#' @param scale "fixed" (the default) for fixed scale. "free" for scaling
#'    conditional on each daily cell, "free_wday" for scaling on weekdays, 
#'    "free_mday" for scaling on day of month.
#' @param width,height Numerics between 0 and 1 to specify the width/height for 
#'    each glyph.
#'
#' @return A data frame or a tibble with newly added columns of `.x`, `.y`. `.x` 
#'    and `.y` together give new coordinates computed for different types of 
#'    calendars. `date` groups the same dates in a chronological order, which is
#'    useful for `geom_line` or `geom_path`. The basic use is `ggplot(aes(x = .x, 
#'    y = .y, group = date)) + geom_*`. The variable names `.x` and `.y` reflect 
#'    the actual `x` and `y` with a prefix `.`.
#'
#' @details The calendar-based graphic can be considered as small multiples
#'    of sub-series arranged into many daily cells. For every multiple (or
#'    facet), it requires the `x` variable mapped to be time of day and `y` to
#'    value. New `x` and `y` are computed and named with a `.` prefixed to variable
#'    according to `x` and `y` respectively, and get ready for `ggplot2` aesthetic 
#'    mappings. In conjunction with `group_by()`, it allows the grouped variable 
#'    to have their individual scales. For more details, see `vignette("frame-calendar",
#'    package = "sugrrants")`
#'
#' @author Earo Wang
#'
#' @rdname frame-calendar
#' 
#' @examples
#'    library(dplyr)
#'    # compute the calendar layout for the data frame
#'    calendar_df <- pedestrian %>%
#'      filter(Sensor_ID == 13) %>% 
#'      frame_calendar(x = Time, y = Hourly_Counts, date = Date, nrow = 4)
#'
#'    # ggplot
#'    p1 <- calendar_df %>% 
#'      ggplot(aes(x = .Time, y = .Hourly_Counts, group = Date)) +
#'      geom_line()
#'    prettify(p1)
#'    
#'    # use in conjunction with group_by()
#'    grped_calendar <- pedestrian %>% 
#'      filter(Year == "2017", Month == "March") %>% 
#'      group_by(Sensor_Name) %>% 
#'      frame_calendar(
#'        x = Time, y = Hourly_Counts, date = Date, sunday = TRUE
#'      )
#'    
#'    p2 <- grped_calendar %>% 
#'      ggplot(aes(x = .Time, y = .Hourly_Counts, group = Date)) +
#'      geom_line() +
#'      facet_wrap(~ Sensor_Name, nrow = 2)
#'    prettify(p2)
#'    \dontrun{
#'      # allow for different languages
#'      # below gives simplied Chinese labels with STKaiti font family,
#'      # assuming this font installed in user's local system
#'      prettify(p2, locale = "zh", family = "STKaiti")
#'    }
#'
#' @export
frame_calendar <- function(
  data, x, y, date, calendar = "monthly", dir = "h", sunday = FALSE, 
  nrow = NULL, ncol = NULL, polar = FALSE, scale = "fixed",
  width = 0.95, height = 0.95
) {
  calendar <- match.arg(calendar, c("monthly", "weekly", "daily"))
  dir <- match.arg(dir, c("h", "v"))
  scale <- match.arg(scale, c("fixed", "free", "free_wday", "free_mday"))

  UseMethod("frame_calendar")
}

#' @export
frame_calendar.grouped_df <- function(
  data, x, y, date, calendar = "monthly", dir = "h", sunday = FALSE, 
  nrow = NULL, ncol = NULL, polar = FALSE, scale = "fixed",
  width = 0.95, height = 0.95
) {
  x <- deparse(substitute(x))
  if (!possibly_quosure(y)) y <- deparse(substitute(y)) else y <- dots2str(y)
  date <- deparse(substitute(date))

  if (!possibly_identity(x)) x <- sym(x)
  if (!possibly_identity(y)) y <- syms(y)
  date <- sym(date)
  cls <- class(data)

  # other attributes obtained from the grouped var of largest group size
  idx_max <- which.max(group_size(data))
  data <- data %>% 
    nest(.key = .calendar_tbl) %>% 
    mutate(.calendar_tbl = map(
      .calendar_tbl, 
      function(data) frame_calendar_(
        data = data, x = x, y = y, date = date,
        calendar = calendar, dir = dir, sunday = sunday, 
        nrow = nrow, ncol = ncol, polar = polar, scale = scale,
        width = width, height = height
      )
    )) 
  xattr <- data$.calendar_tbl[[idx_max]]
  data <- unnest(data)

  return(
    structure(data,
      breaks = get_breaks(xattr),
      minor_breaks = get_minor_breaks(xattr),
      label = get_label(xattr),
      text = get_text(xattr),
      text2 = get_text2(xattr),
      dir = get_dir(xattr),
      calendar = calendar,
      class = c("ggcalendar", cls)
    )
  )
}

#' @export
frame_calendar.default <- function(
  data, x, y, date, calendar = "monthly", dir = "h", sunday = FALSE, 
  nrow = NULL, ncol = NULL, polar = FALSE, scale = "fixed",
  width = 0.95, height = 0.95
) {
  x <- deparse(substitute(x))
  if (!possibly_quosure(y)) y <- deparse(substitute(y)) else y <- dots2str(y)
  date <- deparse(substitute(date))

  if (!possibly_identity(x)) x <- sym(x)
  if (!possibly_identity(y)) y <- syms(y)
  date <- sym(date)

  frame_calendar_(
    data, x = x, y = y, date = date,
    calendar = calendar, dir = dir, sunday = sunday, 
    nrow = nrow, ncol = ncol, polar = polar, scale = scale,
    width = width, height = height
  )
}

# frame_calendar_ takes strings as variable name
frame_calendar_ <- function(
  data, x, y, date, calendar = "monthly", dir = "h", sunday = FALSE, 
  nrow = NULL, ncol = NULL, polar = FALSE, scale = "fixed",
  width = 0.95, height = 0.95
) {
  if (identical(between(width, 0, 1) && between(height, 0, 1), FALSE)) {
    abort("width/height must be between 0 and 1.")
  }
  # data <- arrange(data, !!date) # I don't think I need to change row order
  .x <- paste0(".", quo_name(x))
  # .y <- paste0(".", quo_name(y))
  .date <- quo_name(date)
  cls <- class(data)
  old_cn <- colnames(data)

  # as some variables have been created for the computation,
  # if `data` has those variables, they're likely to be overwritten.
  # a warning of conflicts is thrown away.
  int_vars <- c(".gx", ".gy", ".cx", ".cy", ".ymax", ".ymin")
  if (possibly_identity(x)) int_vars <- c(int_vars, ".x")
  if (possibly_identity(y)) int_vars <- c(int_vars, ".y")

  if (polar) int_vars <- c(int_vars, "theta", "radius")
  if (scale %in% c("free_wday", "free_mday")) int_vars <- c(int_vars, ".day")
  check_vars <- int_vars %in% old_cn
  if (any(check_vars)) {
    str_vars <- int_vars[check_vars]
    abort(paste(
      "The variables including",
      paste(str_vars, collapse = ", "),
      "must be renamed to proceed."
    ))
  }

  date_eval <- sort(eval_tidy(date, data = data))
  if (!("Date" %in% class(date_eval))) {
    abort("'date' must be a 'Date' class.")
  }

  if (calendar != "monthly") {
    if (sunday) {
      inform("The argument sunday only works for the monthly calendar.")
    }
    if (!is.null(nrow) || !is.null(ncol)) {
      inform("The argument nrow/ncol only works for the monthly calendar.")
    }
  }

  class(date_eval) <- c(calendar, class(date_eval))
  cal_layout <- setup_calendar(x = date_eval, dir = dir, sunday = sunday,
    nrow = nrow, ncol = ncol)

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
    dplyr::mutate(!!.date := PANEL)

  # Define a small multiple width and height
  width <- resolution(data$.gx, zero = FALSE) * width
  height <- resolution(data$.gy, zero = FALSE) * height
  margins <- min(c(width, height)) # Month by month margin

  if (calendar == "monthly") {
    data <- data %>% 
      dplyr::group_by(MPANEL) %>% 
      dplyr::mutate(
        .gx = .gx + MCOL * margins,
        .gy = .gy - MROW * margins,
        .cx = .cx + MCOL * margins,
        .cy = .cy - MROW * margins
      ) 
  }

  data <- ungroup(data) # is.null(scale)
  if (scale == "free") {
    data <- dplyr::group_by(data, ROW, COL)
  } else if (scale == "free_wday") {
    data <- data %>% 
      dplyr::mutate(.day = wday(!!date)) %>% 
      dplyr::group_by(.day)
  } else if (scale == "free_mday") {
    data <- data %>% 
      dplyr::mutate(.day = mday(!!date)) %>% 
      dplyr::group_by(.day)
  }

  data <- data %>% 
    dplyr::mutate(
      .ymax = max(!!!y, na.rm = TRUE),
      .ymin = min(!!!y, na.rm = TRUE)
    )
  if (polar) { # polar only support one y
    if (length(y) > 1) {
      message("Only the first 'y' variable is used.")
    }
    .y <- paste0(".", y[[1]])
    data <- data %>% 
      dplyr::mutate(
        theta = 2 * pi * normalise(!!x, xmax = max_na(!!x)),
        radius = normalise(!!!y, xmax = max_na(!!!y)),
        !!.x := .cx + width / 2 * radius * sin(theta),
        !!.y := .cy + height / 2 * radius * cos(theta)
      ) %>% 
      dplyr::select(-c(theta, radius))
  } else {
    fn <- function(x, ymax, ymin) { # temporal function for mutate at
      normalise(x, xmax = max_na(ymax), xmin = min_na(ymin)) * height
    }
    if (possibly_identity(x)) {
      data <- dplyr::mutate(data, .x = .cx)
    } else {
      data <- data %>% 
        dplyr::mutate(
          !!.x := .cx + normalise(!!x, xmax = max_na(!!x)) * width
        )
    }
    if (possibly_identity(y)) {
      data <- dplyr::mutate(data, .y = .cy)
    } else {
      data <- data %>% 
        dplyr::mutate_at(
          .vars = vars(!!!y),
          .funs = funs(zzz = .cy + fn(., .ymax, .ymin))
        )
    }
  }
  # generate breaks and labels for prettify()
  class(cal_grids) <- c(calendar, class(cal_grids))
  data_ref <- gen_reference(
    cal_grids, margins, calendar = calendar, sunday = sunday, dir = dir
  )

  data <- data %>% 
    ungroup() %>% 
    dplyr::select(-(ROW:.cy)) %>% 
    dplyr::select(-c(.ymax, .ymin))
  if (scale %in% c("free_wday", "free_mday")) {
    data <- dplyr::select(data, -.day) # remove .day variable
  }

  # rename y's variables
  y_idx <- ends_with("zzz", vars = colnames(data))
  colnames(data)[y_idx] <- paste0(".", y)

  return(
    structure(data,
      breaks = data_ref$breaks,
      minor_breaks = data_ref$minor_breaks,
      label = data_ref$label,
      text = data_ref$text,
      text2 = data_ref$text2,
      dir = dir,
      calendar = calendar,
      class = c("ggcalendar", cls)
    )
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
  return(out)
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
    mtext <- data.frame(
      x = max(minor_breaks$x) + min_width,
      y = minor_breaks$y + min_height / 2
    )
    # Weekday text at the bottom
    dtext <- data.frame(
      x = minor_breaks$x + min_width / 2,
      y = min(minor_breaks$y)
    )
  } else {
    # Month text positioned at the top of each month panel (dir == "v")
    mtext <- data.frame(
      x = minor_breaks$x,
      y = max(minor_breaks$y) + min_height
    )
    dtext <- data.frame(
      x = min(minor_breaks$x),
      y = minor_breaks$y + min_height / 2
    )
  }
  mtext$mon <- unique_labels
  dtext$label <- seq_len(max(mday(date)))

  return(list(
    breaks = NULL, minor_breaks = minor_breaks,
    label = mtext, text = dtext
  ))
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
    mtext <- data.frame(
      x = max(minor_breaks$x) + min_width,
      y = minor_breaks$y
    )
    # Weekday text at the bottom
    dtext <- data.frame(
      x = minor_breaks$x + min_width / 2,
      y = min(minor_breaks$y)
    )
  } else {
    # Week index text positioned at the top of each week panel (dir == "v")
    mtext <- data.frame(
      x = minor_breaks$x,
      y = max(minor_breaks$y) + min_height
    )
    dtext <- data.frame(
      x = min(minor_breaks$x),
      y = minor_breaks$y + min_height / 2
    )
  }
  mtext$label <- unique_labels
  dtext$day <- gen_wday_index(sunday = FALSE)

  return(list(
    breaks = NULL, minor_breaks = minor_breaks,
    label = mtext, text = dtext
  ))
}

gen_reference.monthly <- function(
  grids, margins, dir = "h", sunday = FALSE, ...
) {
  # Month breaks
  grids <- arrange(grids, PANEL)
  grids <- grids %>% 
    group_by(MPANEL) %>% 
    mutate(
      .gx = .gx + MCOL * margins,
      .gy = .gy - MROW * margins
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
  mtext <- expand.grid2(x = xtext, y = ytext)
  mtext <- mtext[seq_along(unique_idx), , drop = FALSE]
  mtext$mon <- unique_labels
  mtext$year <- substring(unique_idx, first = 1, last = 4)

  # Weekday text
  if (dir == "h") {
    dtext <- data.frame(
      x = minor_breaks$x + min_width / 2,
      y = min(minor_breaks$y)
    )
  } else {
    dtext <- data.frame(
      x = min(minor_breaks$x),
      y = minor_breaks$y + min_height / 2
    )
  }
  dtext$day <- gen_wday_index(sunday = sunday)

  # Day of month text
  mday_text <- data.frame(
    x = grids$.gx, 
    y = grids$.gy + min_height,
    label = mday(grids$PANEL)
  )

  return(list(
    breaks = breaks, minor_breaks = minor_breaks,
    label = mtext, text = dtext, text2 = mday_text
  ))
}

#' @rdname frame-calendar
#' @param plot ggplot object
#' @param label If "label" is specified, it will add month/week text on the 
#'    `ggplot` object, which is actually passed to `geom_label()`. If "text" is
#'    specified, it will add weekday/day of month text on the `ggplot` object,
#'    which is actually passed to `geom_text()`. By default, both "label" and 
#'    "text" are used. If "text2" is specified for the "monthly" calendar only, 
#'    it will add day of month to the `ggplot` object.
#' @param locale ISO 639 language code. The default is "en" (i.e. US English).
#'    See [readr::locale] for more details.
#' @param abbr Logical to specify if the abbreviated version of label should be
#'    used.
#' @param ... Extra arguments passed to `geom_label()` and `geom_text()`
#' @export
prettify <- function(plot, label = c("label", "text"), locale, abbr = TRUE, 
  ...) {
  if (missing(plot)) {
    plot <- last_plot()
  }
  if (!is.ggplot(plot)) {
    abort("'plot' must be a ggplot object.")
  }
  if (!("ggcalendar" %in% class(plot$data))) {
    abort("'prettify' does not know how to handle with this type of data.")
  }
  if (is.null(label)) {
    label_arg <- NULL
  } else {
    label_arg <- match.arg(
      label, c("label", "text", "text2"), 
      several.ok = TRUE
    )
  }
  if (missing(locale)) {
    locale <- "en"
  }
  loc_dn <- locale(date_names = locale)$date_names
  if (abbr) {
    mtext <- loc_dn$mon_ab
    dtext <- loc_dn$day_ab
    # a single letter
    if (locale == "en") dtext <- substring(dtext, first = 1, last = 1)
  } else {
    mtext <- loc_dn$mon
    dtext <- loc_dn$day
  }
  label <- get_label(plot$data)
  text <- get_text(plot$data)
  cal <- get_calendar(plot$data)
  if (cal == "monthly") {
    nyr <- unique.default(label$year)
    seq_label <- mtext[label$mon] 
    if (!has_length(nyr, 1)) seq_label <- paste(seq_label, label$year)
    label <- bind_cols(label, label = seq_label)
    text <- bind_cols(text, label = dtext[text$day])
  } else if (cal == "weekly") {
    text <- bind_cols(text, label = dtext[text$day])
  } else if (cal == "daily") {
    seq_label <- mtext[label$mon] 
    label <- bind_cols(label, label = seq_label)
  }
  breaks <- get_breaks(plot$data)
  minor_breaks <- get_minor_breaks(plot$data)
  dir <- get_dir(plot$data)

  # separate params for geom_label and geom_text from ...
  param_list <- list(...)
  if (has_length(param_list, 0)) {
    label_param <- text_param <- text2_param <- list()
  } else {
    names_param <- names(param_list)
    label_all <- c(GeomLabel$aesthetics(), GeomLabel$parameters(TRUE))
    text_all <- c(GeomText$aesthetics(), GeomText$parameters(TRUE))
    label_param <- param_list[which(names_param %in% label_all)]
    text_param <- text2_param <- param_list[which(names_param %in% text_all)]
  }

  if ("label" %in% label_arg) {
    label_param$data <- label
    label_param$mapping <- aes(x, y, label = label)
    label_param$hjust <- label_param$vjust <- 0
    label_param$inherit.aes <- FALSE
    plot <- plot + 
      do.call(geom_label, label_param)
  }
  if ("text" %in% label_arg) {
    text_param$data <- text
    text_param$mapping <- aes(x, y, label = label)
    text_param$inherit.aes <- FALSE
    if (dir == "h") {
      text_param$nudge_y <- -0.01
      text_param$vjust <- 1
      plot <- plot + 
        do.call(geom_text, text_param)
    } else {
      text_param$nudge_x <- -0.01
      text_param$hjust <- 1
      plot <- plot +
        do.call(geom_text, text_param)
    }
  }
  if ("text2" %in% label_arg) {
    text2 <- get_text2(plot$data)
    if (is.null(text2)) {
      warn("label = 'text2' is ignored for this type of calendar.")
    } else {
      text2_param$data <- text2
      text2_param$mapping <- aes(x, y, label = label)
      text2_param$inherit.aes <- FALSE
      text2_param$nudge_y <- -0.01
      text2_param$hjust <- 0
      text2_param$vjust <- 1
      plot <- plot +
        do.call(geom_text, text2_param)
    }
  }
  plot <- plot + 
    scale_x_continuous(breaks = breaks$x, minor_breaks = minor_breaks$x)
  plot <- plot + 
    scale_y_continuous(breaks = breaks$y, minor_breaks = minor_breaks$y) 
  plot <- plot + 
    theme(
      axis.text = element_blank(), 
      axis.ticks = element_blank(),
      axis.title = element_blank()
    )
  plot
}

# helper functions for frame_calendar
get_breaks <- function(data) {
  attr(data, "breaks")
}

get_minor_breaks <- function(data) {
  attr(data, "minor_breaks")
}

get_label <- function(data) {
  attr(data, "label")
}

get_text <- function(data) {
  attr(data, "text")
}

get_text2 <- function(data) {
  attr(data, "text2")
}

get_dir <- function(data) {
  attr(data, "dir")
}

get_calendar <- function(data) {
  attr(data, "calendar")
}

gen_wday_index <- function(sunday = FALSE) {
  if (sunday) return(1:7) else return(c(2:7, 1))
}

gen_day_breaks <- function(grids) {
  # day breaks
  minor_xbreaks_df <- grids %>% 
    group_by(COL) %>% 
    summarise(
      .xminor_min = min(.gx)
    )
  minor_xbreaks <- minor_xbreaks_df$.xminor_min
  minor_ybreaks_df <- grids %>% 
    group_by(ROW) %>% 
    summarise(
      .yminor_min = min(.gy)
    )
  minor_ybreaks <- minor_ybreaks_df$.yminor_min
  minor_breaks <- list(x = minor_xbreaks, y = minor_ybreaks)
  return(minor_breaks)
}
