# SE
# by_season_ <- function(.data, .index, .value, .f, ..., .label = ".out",
#                        hemisphere = "N", definition = "meterological",
#                        date_format = TRUE) {
#   .f <- match.fun(uq(.f))
#   hemisphere <- match.arg(hemisphere, c("N", "S"))
#   definition <- match.arg(
#     definition, c("meterological", "astronomical", "midsolstice")
#   )
#   add <- if_else(is.grouped_df(.data), TRUE, FALSE)
#   # fun_name <- expr_text(.f)

#   .data <- .data %>% 
#     mutate_(.season = f_interp(
#       ~ as_season(uq(.index), hemisphere = hemisphere, definition = definition,
#                   date_format = date_format))) %>% 
#     group_by_(~ .season, add = add)

#   # retrieve the grouping variables for later
#   grouped_vars <- groups(.data)

#   # by_slice should retain the grouping structure
#   .data <- .data %>% 
#     by_slice(~ .f(f_eval_rhs(.value, .x), ...), .collate = "list", .to = .label) 

#   .data <- .data %>% 
#     unnest() %>% 
#     group_by_(.dots = grouped_vars)

#   return(.data)
# }

# NSE
# by_season <- function(.data, .index, .value, .f, ..., .label = ".out",
#                       hemisphere = "N", definition = "meterological",
#                       date_format = TRUE) {
#   by_season_(.data, f_capture(.index), f_capture(.value), .f, ..., 
#              .label = .label, hemisphere = hemisphere, definition = definition,
#              date_format = date_format)
# }

#' @name by_month
#' @aliases by_week
#' @aliases by_day
#'
#' @title Aggregate over fixed calendar periods
#'
#' @description Apply a specified function to each fixed calendar periods of
#'    a data frame that contains a date-times variable. It can be used in
#'    conjunction with `dplyr::group_by`.
#'
#' @param .data A data frame.
#' @param .index A variable of date-times in the `.data`.
#' @param .value A variable in the `.data` that a specified function to
#'    apply.
#' @param .f A specified function applied to the fixed calendar windows of the
#'    `.value`.
#' @param ... The extra arguments passed to `.f`.
#' @param .label a character string indicates the name of output column.
#'
#' @return An aggregated data frame with the added variables of calendar periods 
#'    (e.g. `.month`, `.week`, `.day`) and the aggregated values 
#'    while keeping grouped variables if present.
#'
#' @details When a specified function `.f` generates more than one values,
#'    it returns a long data form with one stack on the rest.
#'
#' @author Earo Wang
#'
#' @examples
#'    library(dplyr)
#'    pedestrian %>%
#'      group_by(Sensor_Name) %>% 
#'      by_month(.index = Date_Time, .value = Hourly_Counts, .f = quantile,
#'        na.rm = TRUE)
#'
#' @rdname by_month
#' @export
#'
# NSE
by_month <- function(.data, .index, .value, .f, ..., .label = ".out") {
  by_month_(.data, f_capture(.index), f_capture(.value), .f, ..., 
           .label = .label)
}

# SE
by_month_ <- function(.data, .index, .value, .f, ..., .label = ".out") {
  .f <- match.fun(uq(.f))
  add <- if_else(is.grouped_df(.data), TRUE, FALSE)
  # fun_name <- expr_text(.f)

  .data <- .data %>% 
    mutate_(.month = f_interp(~ as_month(uq(.index)))) %>% 
    group_by_(~ .month, add = add)

  # retrieve the grouping variables for later
  grouped_vars <- groups(.data)

  # by_slice should retain the grouping structure
  .data <- .data %>% 
    by_slice(~ .f(f_eval_rhs(.value, .x), ...), .collate = "list", .to = .label) 

  .data <- .data %>% 
    unnest() %>% 
    group_by_(.dots = grouped_vars)

  return(.data)
}

#' @rdname by_month
#' @export
# NSE
by_week <- function(.data, .index, .value, .f, ..., .label = ".out") {
  by_week_(.data, f_capture(.index), f_capture(.value), .f, ..., 
           .label = .label)
}

# by_week is a window function that applying a function to a fixed window of 
# a week.
# If .f returns only one value, it's better to use as_week and summarise
# SE
by_week_ <- function(.data, .index, .value, .f, ..., .label = ".out") {
  .f <- match.fun(uq(.f))
  add <- if_else(is.grouped_df(.data), TRUE, FALSE)
  # fun_name <- expr_text(.f)

  .data <- .data %>% 
    mutate_(.week = f_interp(~ as_week(uq(.index)))) %>% 
    group_by_(~ .week, add = add)

  # retrieve the grouping variables for later
  grouped_vars <- groups(.data)
  # if .label = NULL, remove the .label column in the output
  # is_null_label <- is.null(.label)
  # .label <- if_else(is_null_label, ".out", .label)

  # by_slice should retain the grouping structure
  .data <- .data %>% 
    by_slice(~ .f(f_eval_rhs(.value, .x), ...), .collate = "list", .to = .label) 

  # Unsure whether it's useful to return the labels
  # if (!is_null_label) {
  #   out_labels <- data_frame(
  #     .label = lapply(
  #        f_eval_rhs(f_new(as_name(.label)), .data), 
  #        function(x) {
  #           if (length(x) == 1) {
  #             .label
  #           } else {
  #             paste0(.label, seq_along(x))
  #           }
  #        }
  #      )
  #   )
  #   .data <- bind_cols(.data, out_labels)
  # } 

  .data <- .data %>% 
    unnest() %>% 
    group_by_(.dots = grouped_vars)

  return(.data)
}

#' @rdname by_month
#' @export
# NSE
by_day <- function(.data, .index, .value, .f, ..., .label = ".out") {
  by_day_(.data, f_capture(.index), f_capture(.value), .f, ..., 
           .label = .label)
}

# apply to daily data
# SE
by_day_ <- function(.data, .index, .value, .f, ..., .label = ".out") {
  .f <- match.fun(uq(.f))
  add <- if_else(is.grouped_df(.data), TRUE, FALSE)
  # fun_name <- expr_text(.f)

  .data <- .data %>% 
    mutate_(.day = f_interp(~ as_date(uq(.index)))) %>% 
    group_by_(~ .day, add = add)

  # retrieve the grouping variables for later
  grouped_vars <- groups(.data)

  # by_slice should retain the grouping structure
  .data <- .data %>% 
    by_slice(~ .f(f_eval_rhs(.value, .x), ...), .collate = "list", .to = .label) 

  .data <- .data %>% 
    unnest() %>% 
    group_by_(.dots = grouped_vars)

  return(.data)
}
