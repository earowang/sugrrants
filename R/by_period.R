# SE
by_season_ <- function(.data, .index, .value, .f, ..., .label = ".out",
                       hemisphere = c("N", "S"),
                       definition = c("meterological", "astronomical", 
                                      "midsolstice"),
                       date_format = TRUE) {
  .f <- match.fun(uq(.f))
  hemisphere <- match.arg(hemisphere)
  definition <- match.arg(definition)
  add <- if_else(is.grouped_df(.data), TRUE, FALSE)
  # fun_name <- expr_text(.f)

  .data <- .data %>% 
    mutate_(.season = f_interp(
      ~ on_season(uq(.index), hemisphere = hemisphere, definition = definition,
                  date_format = date_format))) %>% 
    group_by_(~ .season, add = add)

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
#'
#' @param hemisphere A character vector that specifies the hemisphere. "N" refers
#'    to the Northern hemisphere, and "S" to the Souther hemisphere.
#' @param definition A character vector that defines the season range. "meterological"
#'    is the only one supported at the stage.
#'
#' @export
# NSE
by_season <- function(.data, .index, .value, .f, ..., .label = ".out",
                       hemisphere = c("N", "S"),
                       definition = c("meterological", "astronomical", 
                                      "midsolstice"),
                      date_format = TRUE) {
  by_season_(.data, f_capture(.index), f_capture(.value), .f, ..., 
             .label = .label, hemisphere = hemisphere, definition = definition,
             date_format = date_format)
}

# SE
by_month_ <- function(.data, .index, .value, .f, ..., .label = ".out",
                      date_format = TRUE) {
  .f <- match.fun(uq(.f))
  add <- if_else(is.grouped_df(.data), TRUE, FALSE)
  # fun_name <- expr_text(.f)

  .data <- .data %>% 
    mutate_(.month = f_interp(~ on_month(uq(.index), date_format = date_format))) %>% 
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

#' @name by_month
#' @aliases by_season
#' @aliases by_week
#'
#' @title Aggregate over fixed calendar periods
#'
#' @description Aggregate over calendar periods
#'
#' @param .data A data frame.
#' @param .index The variable of date-time objects in the \code{.data}.
#' @param .value The variable of numerics in the \code{.data} to be aggregated.
#' @param .f A function applied to aggregation over the fixed calendar periods.
#' @param ... The extra arguments passed to \code{.f}.
#' @param .label a character string to be labelled for the aggregated values.
#' @param date_format Logical. If TRUE, it returns a date format; otherwise a
#'    character string.
#'
#' @return A data frame that contains the calendar periods and the aggregated values.
#'
#' @author Earo Wang
#' 
#' @rdname by_month
#' @export
#'
# NSE
by_month <- function(.data, .index, .value, .f, ..., .label = ".out",
                     date_format = TRUE) {
  by_month_(.data, f_capture(.index), f_capture(.value), .f, ..., 
           .label = .label, date_format = date_format)
}

# by_week is a window function that applying a function to a fixed window of 
# a week.
# If .f returns only one value, it's better to use on_week and summarise
# SE
by_week_ <- function(.data, .index, .value, .f, ..., .label = ".out",
                     date_format = TRUE) {
  .f <- match.fun(uq(.f))
  add <- if_else(is.grouped_df(.data), TRUE, FALSE)
  # fun_name <- expr_text(.f)

  .data <- .data %>% 
    mutate_(.week = f_interp(~ on_week(uq(.index), date_format = date_format))) %>% 
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
by_week <- function(.data, .index, .value, .f, ..., .label = ".out",
                    date_format = TRUE) {
  by_week_(.data, f_capture(.index), f_capture(.value), .f, ..., 
           .label = .label, date_format = date_format)
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
# NSE
by_day <- function(.data, .index, .value, .f, ..., .label = ".out") {
  by_day_(.data, f_capture(.index), f_capture(.value), .f, ..., 
           .label = .label)
}
