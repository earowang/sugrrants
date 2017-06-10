#' @title Build a calendar view for a time series data frame
#'
#' @description A calendar view is useful to visualise time series at daily intervals
#'    or higher frequency levels. `frame_calendar` sets up the calendar
#'    layout for the input data frame, and the results is ready for `ggplot2`.
#'    Each row represents a week and the first cell in the row indicates Mondays.
#'
#' @param data A data frame.
#' @param x,y  Variables to be passed to `ggplot2(aes(x, y))`.
#' @param date  A variable of date-times that helps to tell the days in the
#'    calendar.
#' @param calendar Character. Type of calendar.
#' @param dir Direction: "h" for horizontal (the default) or "v" for vertical.
#' @param sunday Logical. FALSE (the default) for starting the weekday as Monday
#'    in the monthly calendar, or TRUE for Sunday.
#' @param nrow,ncol Integer. Number of rows and columns for the calendar layout.
#' @param polar Logical. Cartesian (FALSE, the default) or polar (TRUE) coordinates.
#' @param scale "global" (the default) for scaling globally, or "local" for scaling
#     at each day cell.
#'
#' @return A data frame with newly added columns of `.x`, `.y`, and 
#'    `.group_id`
#'
#' @details Calendar view is a special ordered layout that `ggplot2::facet_grid` 
#'    and `ggplot2::facet_wrap` currently do not support. This creates new
#'    coordinates of `(.x, .y)` to place to the correct panel in the calendar 
#'    and new grouped sequence of `.group_id` from `date` using some 
#'    linear algebra.
#'
#' @author Earo Wang
#'
#' @rdname frame-calendar
#' 
#' @examples
#'    library(dplyr)
#'    # get the calendar layout for the data frame
#'    ped_calendar <- pedestrian %>%
#'      filter(Sensor_ID == 13) %>% 
#'      mutate(Time = lubridate::hour(Date_Time)) %>% 
#'      frame_calendar(x = Time, y = Hourly_Counts, date = Date_Time,
#'        nrow = 3, ncol = 4)
#'
#'    # plot
#'    library(ggplot2)
#'    ped_calendar %>% 
#'      ggplot(aes(x = .x, y = .y, group = .group_id)) +
#'      geom_line()
#'    prettify()
#'
#' @export
#'
frame_calendar <- function(
  data, x, y, date, calendar = "monthly", dir = "h", sunday = FALSE, 
  nrow = NULL, ncol = NULL, polar = FALSE, scale = "global"
) {
  x <- enquo(x)
  y <- enquo(y)
  date <- enquo(date)

  grped_data <- is.grouped_df(data)
  grped_vars <- groups(data)
  date_eval <- eval_tidy(date, data = data)

  calendar <- match.arg(calendar, c("monthly", "weekly", "daily"))
  dir <- match.arg(dir, c("h", "v"))
  scale <- match.arg(scale, c("global", "local"))

  start_date <- min_na(as_date(date_eval))

  data <- data %>% 
    arrange(!!date) %>% 
    mutate(.group_id = gen_group_id(!!date, start_date))

  class(date_eval) <- c(calendar, class(date_eval))
  cal_layout <- setup_calendar(x = date_eval, dir = dir, sunday = sunday,
    nrow = nrow, ncol = ncol)

  # Assign grids to the panels
  grids <- assign_grids(max(cal_layout$ROW), max(cal_layout$COL))
  cal_grids <- cal_layout %>% 
    left_join(grids, by = c("COL", "ROW"))

  data <- data %>% 
    left_join(cal_grids, by = c(".group_id" = "PANEL"))

  # Define a small multiple width and height
  width <- resolution(data$.gx, zero = FALSE) * 0.95
  height <- resolution(data$.gy, zero = FALSE) * 0.95
  margins <- mean(c(width, height)) # Month by month margin

  if (calendar == "monthly") {
    data <- data %>% 
      group_by(MPANEL) %>% 
      mutate(
        .gx = .gx + MCOL * margins,
        .gy = .gy - MROW * margins
      ) 
  }

  if (scale == "global") {
    data <- ungroup(data)
  } else { # local scale
    data <- group_by(data, ROW, COL)
  }

  if (polar) {
    data <- data %>% 
      mutate(
        theta = 2 * pi * normalise(!!x, xmax = max_na(!!x)),
        radius = normalise(!!y, xmax = max_na(!!y)),
        .x = .gx + width * radius * sin(theta),
        .y = .gy + height * radius * cos(theta)
      ) %>% 
      select(-c(theta, radius))
  } else {
    data <- data %>% 
      mutate(
        .x = .gx + normalise(!!x, xmax = max_na(!!x)) * width,
        .y = .gy + normalise(!!y, xmax = max_na(!!y)) * height
      )
  }
  # generate breaks and labels for prettify()
  class(cal_grids) <- c(calendar, class(cal_grids))
  data_ref <- gen_reference(
    cal_grids, date = date_eval, margins, calendar = calendar, 
    sunday = sunday, dir = dir, polar = polar
  )

  data <- data %>% 
    ungroup() %>% 
    select(-(ROW:.gy))

  attr(data, "breaks") <- data_ref$breaks
  attr(data, "minor_breaks") <- data_ref$minor_breaks
  attr(data, "mlabel") <- data_ref$mlabel
  attr(data, "dlabel") <- data_ref$dlabel
  attr(data, "dir") <- dir

  if (grped_data) {
    data <- data %>% 
      group_by(!!!grped_vars)
  }
  return(data)
}

## calendar functions -------------------
# Generate group_id for each series, but the same starting point is required 
# across different series. It's determined by empirical data.
gen_group_id <- function(x, start_date) {
  # x is a date object
  date_x <- as_date(x)
  num_x <- as.numeric(date_x)
  start_mday <- mday(start_date)
  start_idx <- ifelse(
    start_mday == 1, 
    as.numeric(start_date), 
    as.numeric(start_date - start_mday + 1)
  )
  group_id <- num_x - start_idx + 1
  return(group_id)
}

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
      y = minor_breaks$y + min_height / 2
    )
    # Weekday text at the bottom
    dtext <- data.frame(
      x = minor_breaks$x + min_width / 2,
      y = min(minor_breaks$y)
    )
  } else {
    # Week index text positioned at the top of each week panel (dir == "v")
    mtext <- data.frame(
      x = minor_breaks$x + min_width / 2,
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
    mlabel = mtext, dlabel = dtext
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
    mlabel = mtext, dlabel = dtext
  ))
}

#' @rdname frame-calendar
#' @param plot ggplot object
#' @param ... Extra arguments passed to geom_label and geom_text
#' @export
prettify <- function(plot, ...) {
  if (missing(plot)) {
    plot <- last_plot()
  }
  if (!is.ggplot(plot)) {
    abort("'plot' must be a ggplot object.")
  }
  mlabel <- get_mlabel(plot$data)
  dlabel <- get_dlabel(plot$data)
  breaks <- get_breaks(plot$data)
  minor_breaks <- get_minor_breaks(plot$data)
  dir <- get_dir(plot$data)

  plot <- plot + 
    geom_label(
      aes(x, y, label = label), data = mlabel,
      hjust = 0, vjust = 0, inherit.aes = FALSE,
      ...
    )
  if (dir == "h") {
    plot <- plot + 
      geom_text(
        aes(x, y, label = label), data = dlabel,
        nudge_y = -0.01, vjust = 1, inherit.aes = FALSE, ...
      )
  } else {
    plot <- plot + 
      geom_text(
        aes(x, y, label = label), data = dlabel,
        nudge_x = -0.01, hjust = 1, inherit.aes = FALSE, ...
      )
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

get_mlabel <- function(data) {
  attr(data, "mlabel")
}

get_dlabel <- function(data) {
  attr(data, "dlabel")
}

get_dir <- function(data) {
  attr(data, "dir")
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
