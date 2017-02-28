globalVariables(c(".group_id", ".x", ".y", ".gx", ".gy", "COL", "MCOL",
  "MPANEL", "MROW", "PANEL", ".xminor_min", ".xmajor_min", ".ymajor_max",
  ".ymajor_min", ".yminor_min", "ROW", "minor_breaks"))

#' @title Build a calendar view for a time series data frame
#'
#' @description A calendar view is useful to visualise time series at daily intervals
#'    or higher frequency levels. `frame_calendar` sets up the calendar
#'    layout for the input data frame, and the results is ready for `ggplot2`.
#'    Each row represents a week and the first cell in the row indicates Mondays.
#'
#' @param .data A data frame.
#' @param x,y  Variables to be passed to `ggplot2(aes(x, y))`.
#' @param date  A variable of date-times that helps to tell the days in the
#'    calendar.
#' @param nrow,ncol Number of rows and columns for the calendar layout.
#' @param reference_frames Logical. If `TRUE`, it returns reference lines for days
#'    and months blocks, and month labels.
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
#'
#' @export
#'
frame_calendar <- function(.data, x, y, date, nrow = NULL, ncol = NULL,
  reference_frames = FALSE) {
  frame_calendar_(.data, f_capture(x), f_capture(y), f_capture(date),
    nrow = nrow, ncol = ncol, reference_frames = reference_frames)
}

frame_calendar_ <- function(.data, x, y, date, nrow = NULL, ncol = NULL,
  reference_frames = FALSE) {
  # Prepare the important information
  grouped_vars <- groups(.data)
  x <- f_eval_rhs(x, .data)
  y <- f_eval_rhs(y, .data)
  dates <- f_eval_rhs(~ as_date(uq(date)), data = .data)

  # Use unique date to generate .group_id
  num_dates <- as.numeric(dates)
  start_date <- min(num_dates, na.rm = TRUE)
  end_date <- max(num_dates, na.rm = TRUE)
  id_actual <- num_dates - start_date + 1
  id_range <- rep(seq_len(end_date - start_date + 1), each = length(unique(x)))
  id_dates <- id_range[match(id_actual, id_range)]
  .data <- .data %>% 
    mutate(.group_id = id_dates)

  # Set up the calendar layout and panels
  full_layout <- setup_calendar_layout(dates, nrow = nrow, ncol = ncol)
  sel_panels <- setup_calendar_panel(dates)
  sel_layout <- full_layout %>% 
    filter(PANEL %in% sel_panels) %>% 
    mutate(.group_id = seq_along(sel_panels))

  # Assign correct grids to the panels
  grids <- assign_grids(max(sel_layout$ROW), max(sel_layout$COL))
  sel_grids <- sel_layout %>% 
    left_join(grids, by = c("COL", "ROW"))

  .data <- .data %>% 
    left_join(sel_grids, by = ".group_id")

  # Define a small multiple width and height
  width <- resolution(.data$.gx, zero = FALSE) * 0.95
  height <- resolution(.data$.gy, zero = FALSE) * 0.95
  margins <- mean(c(width, height)) # Month by month margin

  check <- length(unique(x)) == 1 # x is constant, cannot be normalised
  # Rescale using some linear algebra
  .data <- .data %>% 
    group_by(MPANEL) %>% 
    mutate(
      .gx = .gx + MCOL * margins,
      .gy = .gy - MROW * margins
    ) %>% 
    ungroup()
  if (check) {
    .data <- .data %>% 
      mutate(
        .x = .gx + x * width,
        .y = .gy + normalise(y) * height
      )
  } else {
    .data <- .data %>% 
      mutate(
        .x = .gx + normalise(x) * width,
        .y = .gy + normalise(y) * height
      )
  }
  # group_by_(.dots = grouped_vars)

  if (reference_frames) { # add reference lines around days and months
    # Month breaks
    xbreaks_df <- .data %>% 
      group_by(MCOL) %>% 
      summarise(
        .xmajor_min = min(.x, na.rm = TRUE)
      ) %>% 
      distinct(.xmajor_min)
    xbreaks <- fast_unlist(xbreaks_df)
    ybreaks_df <- .data %>% 
      group_by(MROW) %>% 
      summarise(
        .ymajor_min = min(.y, na.rm = TRUE)
      ) %>% 
      distinct(.ymajor_min)
    ybreaks <- fast_unlist(ybreaks_df)

    # day breaks
    minor_xbreaks_df <- .data %>% 
      group_by(COL) %>% 
      summarise(
        .xminor_min = min(.x, na.rm = TRUE)
      ) %>% 
      distinct(.xminor_min)
    minor_xbreaks <- fast_unlist(minor_xbreaks_df)
    minor_ybreaks_df <- .data %>% 
      group_by(ROW) %>% 
      summarise(
        .yminor_min = min(.y, na.rm = TRUE)
      ) %>% 
      distinct(.yminor_min)
    minor_ybreaks <- fast_unlist(minor_ybreaks_df)

    .breaks <- expand.grid(x = xbreaks, y = ybreaks,
      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    .minor_breaks <- expand.grid(x = minor_xbreaks, y = minor_ybreaks,
      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    breaks <- list(breaks = .breaks, minor_breaks = .minor_breaks)

    # Month text
    xtext_df <- .data %>% 
      group_by(MCOL) %>% 
      summarise(
        .xmajor_min = min(.x, na.rm = TRUE)
      ) %>% 
      distinct(.xmajor_min)
    xtext <- fast_unlist(xtext_df)
    ytext_df <- .data %>% 
      group_by(MROW) %>% 
      summarise(
        .ymajor_max = max(.y, na.rm = TRUE) + margins / 2
      ) %>% 
      distinct(.ymajor_max)
    ytext <- sort(fast_unlist(ytext_df), decreasing = TRUE)

    yrs <- year(dates)
    nyears <- unique(yrs)
    month_labels <- paste(yrs, month(dates, label = TRUE), sep = "-")
    unique_labels <- substr(unique(month_labels), start = 6, stop = 8)

    .text <- expand.grid(x = xtext, y = ytext, 
      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    .text$label <- unique_labels

    .data <- .data %>% 
      select(-(COL:.gy))

    out <- list(data = .data, breaks = breaks, label = .text)
    return(out)
  } else {
    .data <- .data %>% 
      select(-(COL:.gy))

    return(.data)
  }
}

# Extract the number of days for a month
days_of_month <- function(x) { # x is a date object
  mth <- formatC(month(x), width = 2, flag = "0")
  ndays <- switch(
    mth,
    "01" = 31, "02" = 28 + leap_year(x), 
    "03" = 31, "04" = 30, 
    "05" = 31, "06" = 30, 
    "07" = 31, "08" = 31, 
    "09" = 30, "10" = 31, 
    "11" = 30, "12" = 31
  )
  return(ndays)
}

# Setup the basic layout with 7 days * 5 weeks in a month
setup_calendar_layout <- function(x, nrow = NULL, ncol = NULL) {
  dates <- unique(as_month(x))
  nweeks <- 5
  nwdays <- 7
  combs <- expand.grid(
    COL = seq_len(nwdays), ROW = c(seq_len(nweeks), 1),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  ncells <- (nweeks + 1) * nwdays
  npanels <- nrow * ncol
  cal_layout <- vector(mode = "list", length = npanels)
  panels <- expand.grid(
    COL = seq_len(ncol), ROW = seq_len(nrow),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  for (i in seq_len(npanels)) {
    col_modulus <- panels[i, "COL"] %% ncol
    col_modulus <- if_else(col_modulus == 0, ncol, col_modulus)
    row_modulus <- panels[i, "ROW"] %% nrow
    row_modulus <- if_else(row_modulus == 0, nrow, row_modulus)
    cal_layout[[i]] <- combs
    cal_layout[[i]]$MCOL <- panels[i, "COL"]
    cal_layout[[i]]$MROW <- panels[i, "ROW"]
    cal_layout[[i]]$COL <- cal_layout[[i]]$COL + nwdays * (col_modulus - 1)
    cal_layout[[i]]$ROW <- cal_layout[[i]]$ROW + nweeks * (row_modulus - 1)
    cal_layout[[i]]$MPANEL <- i
  }

  nmonths <- length(dates)
  if (npanels != nmonths) {
    cal_layout[(nmonths + 1):npanels] <- NULL
  }

  cal_layout <- bind_rows(cal_layout)
  cal_layout$PANEL <- seq_len(ncells * nmonths)
  return(cal_layout)
}

# Actual calendar that the date-time obj falls
setup_calendar <- function(x) { # x is a date-time object
  first_day <- as_month(x)
  start_wday <- wday2(first_day)
  ndays <- days_of_month(x = first_day)
  gen_wdays <- wday2(c(
    first_day, first_day + days(seq_len(ndays - 1))), label = TRUE
  )

  # Set up the index for gtable (5 by 7)
  nweeks <- 5
  nwdays <- 7
  remainders <- start_wday + ndays - 8
  modulus <- remainders %% nwdays
  remainders_wks <- floor(remainders / nwdays)
  first_row <- rep(1, (nwdays - start_wday + 1))
  middle_rows <- rep(2:(remainders_wks + 1), each = nwdays)
  first_col <- start_wday:nwdays
  middle_cols <- rep(1:nwdays, remainders_wks)
  if (modulus == 0) {
    row_idx <- c(first_row, middle_rows)
    col_idx <- c(first_col, middle_cols)
  } else {
    # shift the 6th week data to the first row
    row_idx <- c(first_row, middle_rows, rep(1, modulus))
    # using maximum 6 weeks
    # row_idx <- c(first_row, middle_rows, rep(remainders_wks + 2, modulus))
    col_idx <- c(first_col, middle_cols, seq_len(modulus))
  }
  sel_panels <- seq_along(row_idx) + start_wday - 1
  cal_table <- tibble(
    PANEL = sel_panels,
    ROW = row_idx,
    COL = col_idx
  )
  return(cal_table)
}

# Find the correct panel
setup_calendar_panel <- function(x) {
  yr_month <- unique(as_month(x))
  nfacets <- length(yr_month)
  seq_facets <- seq_len(nfacets)
  base_layout <- lapply(yr_month, setup_calendar)
  gaps <- seq.int(0, by = 42, length.out = nfacets)
  incr_panels <- unlist(
    map2(base_layout, gaps, ~ .x$PANEL + .y),
    recursive = FALSE, use.names = FALSE
  )
  return(incr_panels)
}

assign_grids <- function(ROW, COL) {
  col_grids <- seq(1, 0, length.out = ROW)
  row_grids <- seq(0, 1, length.out = COL)
  grids <- expand.grid(
    .gx = row_grids, .gy = col_grids,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  combs <- expand.grid(
    COL = seq_len(COL), ROW = seq_len(ROW),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  out <- cbind(combs, grids)
  return(out)
}

# Normalise the numerics to range from 0 to 1
normalise <- function(x) {
  return((x - min(x, na.rm = TRUE)) / diff(range(x, na.rm = TRUE)))
}

# Fast unlist (a wrapper of unlist)
fast_unlist <- function(x) {
  return(unlist(x, recursive = FALSE, use.names = FALSE))
}
