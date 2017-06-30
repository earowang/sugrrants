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
#' @param x A variable mapping to x axis, for example time of day. If integer 1 
#'    is specified, it simply returns calendar grids on x without transformation.
#' @param y One variable or more mapping to y axis. If more than one variable,
#'    the variables needs to be quoted. If integer 1 is specified, it returns
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
#'    value. New `x` and `y` are computed and named with a prefix `.`
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
#'    grped_calendar %>% 
#'      ggplot(aes(x = .Time, y = .Hourly_Counts, group = Date)) +
#'      geom_line() +
#'      facet_wrap(~ Sensor_Name, nrow = 2)
#'    # prettify() not working for group_by
#'
#' @export
frame_calendar <- function(
  data, x, y, date, calendar = "monthly", dir = "h", sunday = FALSE, 
  nrow = NULL, ncol = NULL, polar = FALSE, scale = "fixed"
) {
  calendar <- match.arg(calendar, c("monthly", "weekly", "daily"))
  dir <- match.arg(dir, c("h", "v"))
  scale <- match.arg(scale, c("fixed", "free", "free_wday", "free_mday"))

  UseMethod("frame_calendar")
}

#' @export
frame_calendar.grouped_df <- function(
  data, x, y, date, calendar = "monthly", dir = "h", sunday = FALSE, 
  nrow = NULL, ncol = NULL, polar = FALSE, scale = "fixed"
) {
  if (!possibly_string(x)) x <- deparse(substitute(x))
  if (!possibly_string(y)) y <- deparse(substitute(y))
  if (!possibly_string(date)) date <- deparse(substitute(date))

  if (!possibly_identity(x)) x <- sym(x)
  if (!possibly_identity(y)) y <- syms(y)
  date <- sym(date)

  data <- data %>% 
    nest(.key = .calendar_tbl) %>% 
    mutate(.calendar_tbl = map(
      .calendar_tbl, 
      function(data) frame_calendar_(
        data = data, x = x, y = y, date = date,
        calendar = calendar, dir = dir, sunday = sunday, 
        nrow = nrow, ncol = ncol, polar = polar, scale = scale
      )
    )) %>% 
    unnest()
  return(data)
}

#' @export
frame_calendar.default <- function(
  data, x, y, date, calendar = "monthly", dir = "h", sunday = FALSE, 
  nrow = NULL, ncol = NULL, polar = FALSE, scale = "fixed"
) {
  if (!possibly_string(x)) x <- deparse(substitute(x))
  if (!possibly_string(y)) y <- deparse(substitute(y))
  if (!possibly_string(date)) date <- deparse(substitute(date))

  if (!possibly_identity(x)) x <- sym(x)
  if (!possibly_identity(y)) y <- syms(y)
  date <- sym(date)

  frame_calendar_(
    data, x = x, y = y, date = date,
    calendar = calendar, dir = dir, sunday = sunday, 
    nrow = nrow, ncol = ncol, polar = polar, scale = scale
  )
}

frame_calendar_ <- function(
  data, x, y, date, calendar = "monthly", dir = "h", sunday = FALSE, 
  nrow = NULL, ncol = NULL, polar = FALSE, scale = "fixed"
) {
  # data <- arrange(data, !!date) # I don't think I need to change row order
  .x <- paste0(".", quo_name(x))
  # .y <- paste0(".", quo_name(y))
  .date <- quo_name(date)
  cls <- class(data)

  date_eval <- sort(eval_tidy(date, data = data))
  if (type_sum(date_eval) != "date") {
    abort("'date' must be a 'Date' class.")
  }

  class(date_eval) <- c(calendar, class(date_eval))
  cal_layout <- setup_calendar(x = date_eval, dir = dir, sunday = sunday,
    nrow = nrow, ncol = ncol)

  # Assign grids to the panels
  grids <- assign_grids(max(cal_layout$ROW), max(cal_layout$COL))
  cal_grids <- cal_layout %>% 
    left_join(grids, by = c("COL", "ROW"))

  # ideally should use left_join as keeping the colnames in the supplied order
  # but quo_name() doesn't support LHS
  data <- cal_grids %>% 
    right_join(data, by = c("PANEL" = quo_name(date))) %>%
    dplyr::mutate(!!.date := PANEL)

  # Define a small multiple width and height
  width <- resolution(data$.gx, zero = FALSE) * 0.95
  height <- resolution(data$.gy, zero = FALSE) * 0.95
  margins <- min(c(width, height)) # Month by month margin

  if (calendar == "monthly") {
    data <- data %>% 
      dplyr::group_by(MPANEL) %>% 
      dplyr::mutate(
        .gx = .gx + MCOL * margins,
        .gy = .gy - MROW * margins
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
        !!.x := .gx + width * radius * sin(theta),
        !!.y := .gy + height * radius * cos(theta)
      ) %>% 
      dplyr::select(-c(theta, radius))
  } else {
    fn <- function(x, ymax, ymin) { # temporal function for mutate at
      normalise(x, xmax = max_na(ymax), xmin = min_na(ymin)) * height
    }
    if (possibly_identity(x)) {
      data <- dplyr::mutate(data, .x = .gx)
    } else {
      data <- data %>% 
        dplyr::mutate(
          !!.x := .gx + normalise(!!x, xmax = max_na(!!x)) * width
        )
    }
    if (possibly_identity(y)) {
      data <- dplyr::mutate(data, .y = .gy)
    } else {
      data <- data %>% 
        dplyr::mutate_at(
          .vars = vars(!!!y),
          .funs = funs(zzz = .gy + fn(., .ymax, .ymin))
        )
    }
  }
  # generate breaks and labels for prettify()
  class(cal_grids) <- c(calendar, class(cal_grids))
  data_ref <- gen_reference(
    cal_grids, date = date_eval, margins, calendar = calendar, 
    sunday = sunday, dir = dir, polar = polar
  )

  data <- data %>% 
    ungroup() %>% 
    dplyr::select(-(ROW:.gy)) %>% 
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
      dir = dir,
      polar = polar,
      class = c("ggcalendar", cls)
    )
  )
}

## calendar functions -------------------
# Assign grids from 0 to 1
assign_grids <- function(ROW, COL) {
  col_grids <- seq(1, 0, length.out = ROW)
  row_grids <- seq(0, 1, length.out = COL)
  grids <- expand.grid2(.gx = row_grids, .gy = col_grids)
  combs <- expand.grid2(COL = seq_len(COL), ROW = seq_len(ROW))
  out <- cbind(combs, grids)
  return(out)
}

# Compute grid lines and text labels for frame_calendar()
# ToDo: polar = TRUE
gen_reference <- function(grids, date, dir = "h", polar = FALSE, ...) {
  dir <- match.arg(dir, c("h", "v"))
  UseMethod("gen_reference")
}

gen_reference.daily <- function(grids, date, dir = "h", polar = FALSE, ...) {
  # day breaks
  minor_breaks <- gen_day_breaks(grids)
  min_width <- min(abs(diff(minor_breaks$x)))
  min_height <- min(abs(diff(minor_breaks$y)))

  # Prepare for the string texts
  yrs <- year(date)
  nyears <- unique(yrs)
  month_labels <- paste(yrs, month(date, label = TRUE), sep = "-")
  unique_labels <- substring(unique(month_labels), first = 6)

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
  mtext$label <- unique_labels
  dtext$label <- seq_len(max(mday(date)))

  return(list(
    breaks = NULL, minor_breaks = minor_breaks,
    label = mtext, text = dtext
  ))
}

gen_reference.weekly <- function(grids, date, dir = "h", polar = FALSE, ...) {
  # day breaks
  minor_breaks <- gen_day_breaks(grids)
  min_width <- min(abs(diff(minor_breaks$x)))
  min_height <- min(abs(diff(minor_breaks$y)))

  # Prepare for the string texts
  yrs <- year(date)
  nyears <- unique(yrs)
  week_labels <- paste(yrs, isoweek(date), sep = "-")
  unique_labels <- formatC( # make week index to be fixed-width 2 digits
    as.integer(substring(unique(week_labels), first = 6)), 
    width = 2, flag = "0"
  )

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
  dtext$label <- gen_wday_labels(sunday = FALSE)

  return(list(
    breaks = NULL, minor_breaks = minor_breaks,
    label = mtext, text = dtext
  ))
}

gen_reference.monthly <- function(
  grids, date, margins, dir = "h", sunday = FALSE, polar = FALSE, ...
) {
  # Month breaks
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
  min_width <- min(abs(diff(minor_breaks$x)))
  min_height <- min(abs(diff(minor_breaks$y)))

  # month breaks update
  xbreaks <- c(xbreaks_df$.xmajor_min, xbreaks_df$.xmajor_max + min_width)
  ybreaks <- c(ybreaks_df$.ymajor_min, ybreaks_df$.ymajor_max + min_height)
  breaks <- list(x = xbreaks, y = ybreaks)

  # Prepare for the string texts
  yrs <- year(date)
  nyears <- unique(yrs)
  month_labels <- paste(yrs, month(date, label = TRUE), sep = "-")
  unique_labels <- substring(unique(month_labels), first = 6)

  # Month label positioned at the top left of each month panel
  xtext <- sort(xbreaks_df$.xmajor_min)
  ytext <- sort(ybreaks_df$.ymajor_max + min_height, decreasing = TRUE)
  mtext <- expand.grid2(x = xtext, y = ytext)
  mtext <- mtext[seq_along(unique_labels), , drop = FALSE]
  mtext$label <- unique_labels

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
  dtext$label <- gen_wday_labels(sunday = sunday)

  return(list(
    breaks = breaks, minor_breaks = minor_breaks,
    label = mtext, text = dtext
  ))
}

#' @rdname frame-calendar
#' @param plot ggplot object
#' @param label If "label" is specified, it will add month/week text on the 
#'    `ggplot` object, which is actually passed to `geom_label`. If "text" is
#'    specified, it will add weekday/day of month text on the `ggplot` object,
#'    which is actually passed to `geom_text`. By default, both "label" and 
#'    "text" are used.
#' @param ... Extra arguments passed to geom_label and geom_text
#' @export
prettify <- function(plot, label = c("label", "text"), ...) {
  if (missing(plot)) {
    plot <- last_plot()
  }
  if (!is.ggplot(plot)) {
    abort("'plot' must be a ggplot object.")
  }
  if (is.null(label)) {
    label_arg <- NULL
  } else {
    label_arg <- match.arg(label, c("label", "text"), several.ok = TRUE)
  }
  if (!("ggcalendar" %in% class(plot$data))) {
    abort("'prettify' does not know how to handle with this type of data.")
  }
  label <- get_label(plot$data)
  text <- get_text(plot$data)
  breaks <- get_breaks(plot$data)
  minor_breaks <- get_minor_breaks(plot$data)
  dir <- get_dir(plot$data)

  if ("label" %in% label_arg) {
    plot <- plot + 
      geom_label(
        aes(x, y, label = label), data = label,
        hjust = 0, vjust = 0, inherit.aes = FALSE,
        ...
      )
  }
  if ("text" %in% label_arg) {
    if (dir == "h") {
      plot <- plot + 
        geom_text(
          aes(x, y, label = label), data = text,
          nudge_y = -0.01, vjust = 1, inherit.aes = FALSE, ...
        )
    } else {
      plot <- plot + 
        geom_text(
          aes(x, y, label = label), data = text,
          nudge_x = -0.01, hjust = 1, inherit.aes = FALSE, ...
        )
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

get_dir <- function(data) {
  attr(data, "dir")
}

get_polar <- function(data) {
  attr(data, "polar")
}

gen_wday_labels <- function(..., sunday = FALSE) { # simply ignore ...
  wday_labels <- c("M", "T", "W", "T", "F", "S", "S")
  if (sunday) {
    wday_labels <- wday_labels[c(2:7, 1)]
  }
  return(wday_labels)
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
