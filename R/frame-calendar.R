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
#' @rdname calendar
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
frame_calendar <- function(data, x, y, date, calendar = "monthly", dir = "h",
  sunday = FALSE, nrow = NULL, ncol = NULL, polar = FALSE, scale = "global") {
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

  class(date_eval) <- c("cal_monthly", class(date_eval))

  cal_layout <- setup_calendar(date_eval, dir = dir, sunday = sunday,
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

  data <- data %>% 
    group_by(MPANEL) %>% 
    mutate(
      .gx = .gx + MCOL * margins,
      .gy = .gy - MROW * margins
    ) 

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
  data_ref <- gen_reference(data, date = date_eval, margins, calendar = calendar,
    sunday = sunday, dir = dir, polar = polar)

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
# Generate group_id for each series identifier, but need the same starting point
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

# The following snippet for linear coord
gen_reference <- function(data, date, margins, calendar = "monthly", dir = "h", 
  sunday = FALSE, polar = FALSE) {
  # Month breaks
  xbreaks_df <- data %>% 
    group_by(MCOL) %>% 
    summarise(
      .xmajor_min = min_na(.x)
    ) %>% 
    distinct(.xmajor_min)
  xbreaks <- unlist2(xbreaks_df)
  ybreaks_df <- data %>% 
    group_by(MROW) %>% 
    summarise(
      .ymajor_min = min_na(.y)
    ) %>% 
    distinct(.ymajor_min)
  ybreaks <- unlist2(ybreaks_df)

  # day breaks
  minor_xbreaks_df <- data %>% 
    group_by(COL) %>% 
    summarise(
      .xminor_min = min_na(.x)
    ) %>% 
    distinct(.xminor_min)
  minor_xbreaks <- unlist2(minor_xbreaks_df)
  minor_xbreaks_df2 <- data %>% # used for horizontal labelling
    group_by(COL) %>% 
    summarise(
      .xminor_max = max_na(.x)
    ) %>% 
    distinct(.xminor_max)
  minor_xbreaks2 <- unlist2(minor_xbreaks_df2)
  minor_ybreaks_df <- data %>% 
    group_by(ROW) %>% 
    summarise(
      .yminor_min = min_na(.y)
    ) %>% 
    distinct(.yminor_min)
  minor_ybreaks <- unlist2(minor_ybreaks_df)
  minor_ybreaks_df2 <- data %>% 
    group_by(ROW) %>% 
    summarise(
      .yminor_max = max_na(.y)
    ) %>% 
    distinct(.yminor_max)
  minor_ybreaks2 <- unlist2(minor_ybreaks_df2)

  # Month break
  breaks <- expand.grid2(x = xbreaks, y = ybreaks) 
  # Day break
  minor_breaks <- expand.grid2(x = minor_xbreaks, y = minor_ybreaks)

  # Month text
  xtext_df <- data %>% 
    group_by(MCOL) %>% 
    summarise(
      .xmajor_min = min_na(.x)
    ) %>% 
    distinct(.xmajor_min)
  xtext <- unlist2(xtext_df)
  ytext_df <- data %>% 
    group_by(MROW) %>% 
    summarise(
      .ymajor_max = max_na(.y)
    ) %>% 
    distinct(.ymajor_max)
  ytext <- sort(unlist2(ytext_df), decreasing = TRUE)

  yrs <- year(date)
  nyears <- unique(yrs)
  month_labels <- paste(yrs, month(date, label = TRUE), sep = "-")
  unique_labels <- substr(unique(month_labels), start = 6, stop = 8)

  mtext <- expand.grid2(x = xtext, y = ytext)
  mtext <- mtext[seq_along(unique_labels), , drop = FALSE]
  mtext$label <- unique_labels

  # Weekday text
  if (dir == "h") {
    dtext <- tibble(
      x = (minor_xbreaks + minor_xbreaks2) / 2, 
      y = min(ybreaks) - margins / 2
    )
  } else {
    dtext <- tibble(
      x = min(xbreaks) - margins / 2,
      y = (minor_ybreaks + minor_ybreaks2) / 2
    )
  }
  wday_labels <- c("M", "T", "W", "T", "F", "S", "S")
  if (sunday) {
    wday_labels <- wday_labels[c(7, 1:6)]
  }
  dtext$label <- wday_labels
  return(list(
    breaks = breaks, minor_breaks = minor_breaks,
    mlabel = mtext, dlabel = dtext
  ))
}

#' @rdname calendar
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
  mlabel_df <- get_mlabel(plot$data)
  dlabel_df <- get_dlabel(plot$data)
  breaks_df <- get_breaks(plot$data)
  minors_df <- get_minor_breaks(plot$data)
  dir <- get_dir(plot$data)

  plot <- plot + 
    geom_label(
      aes(x, y, label = label), data = mlabel_df,
      nudge_y = 0.01, hjust = 0, vjust = 0, 
      inherit.aes = FALSE,
      ...
    )
  if (dir == "h") {
    plot <- plot + 
      geom_text(
        aes(x, y, label = label), data = dlabel_df,
        vjust = 1, inherit.aes = FALSE, ...
      )
  } else {
    plot <- plot + 
      geom_text(
        aes(x, y, label = label), data = dlabel_df,
        inherit.aes = FALSE, ...
      )
  }
  plot <- plot + 
    scale_x_continuous(breaks = breaks_df$x, minor_breaks = minors_df$x)
  plot <- plot + 
    scale_y_continuous(breaks = breaks_df$y, minor_breaks = minors_df$y) 
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

