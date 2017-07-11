#' Return rows with matching conditions
#'
#' @param .data A `tbl_ts`
#' @param ... Logical expression.
#'
#' @return A tsibble
#' @author Earo Wang
#' @seealso [dplyr::filter]
#' @export
#'
#' @examples
#'    # Leave blank
filter.tbl_ts <- function(.data, ...) {
  key <- get_key(.data)
  index <- get_index(.data)
  interval <- get_interval(.data)
  cls <- class(.data)
  .data <- NextMethod()
  return(structure(
    .data, key = key, index = index, interval = interval, class = cls
  ))
}

#' Select variables by name
#'
#' @param .data A `tbl_ts`.
#' @param ... Unquoted variables to be selected.
#'
#' @return A tsibble when keeping the key and the index variables.
#' @author Earo Wang
#' @seealso [dplyr::select]
#' @export
#'
#' @examples
#'    # Leave blank
select.tbl_ts <- function(.data, ...) {
  cls <- class(.data)
  key <- get_key(.data)
  index <- get_index(.data)
  interval <- get_interval(.data)
  .data <- NextMethod()
  dots_cap <- quos(...)
  idx_there <- any(map_lgl(dots_cap, function(x) x == index))
  key_there <- any(rlang::flatten_lgl(map(key, function(x)
    map_lgl(dots_cap, function(y) y == x)
  )))
  if (idx_there && key_there) {
    return(structure(
      .data, key = key, index = index, interval = interval, class = cls
    ))
  } else {
    return(structure(.data, class = cls[-1]))
  }
}

#' Add new variables
#'
#' @param .data A `tbl_ts`.
#' @param ... Name-value pairs of expressions.
#'
#' @return A tsibble.
#' @author Earo Wang
#' @seealso [dplyr::mutate]
#' @export
#'
#' @examples
#'    # Leave blank
mutate.tbl_ts <- function(.data, ...) {
  key <- get_key(.data)
  index <- get_index(.data)
  interval <- get_interval(.data)
  cls <- class(.data)
  .data <- NextMethod()
  return(structure(
    .data, key = key, index = index, interval = interval, class = cls
  ))
}

#' Group by one or more variabels
#'
#' @param .data A `tbl_ts`.
#' @param ... Unquoted variables to be grouped by.
#' @param add Logical. `FALSE` is the default suggesting to override the exisiting
#'    groups. `TRUE` for adding the exisiting groups.
#'
#' @return A grouped tsibble
#' @author Earo Wang
#' @seealso [dplyr::group_by]
#' @export
#'
#' @examples
#'    # Leave blank
group_by.tbl_ts <- function(.data, ..., add = FALSE) {
  key <- get_key(.data)
  index <- get_index(.data)
  interval <- get_interval(.data)
  .data <- NextMethod(.Generic, object = .data, add = add)
  cls <- c("tbl_ts", class(.data))
  return(structure(
    .data, key = key, index = index, interval = interval, class = cls
  ))
}

#' @title Aggregate over calendar periods
#'
#' @description It computes summary statistics for a tsibble over calendar
#'    periods, usually used in combination of [group_by].
#'
#' @param .data A tsibble (of `tbl_ts` class).
#' @param ... Name-value pairs of summary functions. To aggregate tsibble over
#'    a certain calendar period, for example yearly aggregates, `~ year()` needs
#'    passing to `...`. Please see details.
#'
#' @author Earo Wang
#' @rdname summarise
#' @seealso [dplyr::summarise]
#' @details It's S3 method implemented for [tsibble()] (`tbl_ts`) obtained from
#'    [dplyr::summarise()]. A formula with `~` followed by one of calendar component
#'    functions from base, [lubridate] and [zoo] specifies the period when summary
#'    functions are carried out.  Currently `~ year()` indicates yearly aggregates.
#'    `~ yearqtr()` indicates quarterly aggregates. `~ yearmon()` indicates
#'    monthly aggregates. `~ as_date()` or `as.Date()` indicates daily aggregates.
#' @return A tsibble class when the `~` is present.
#'
#' @examples
#'    # Leave blank
#'
#' @export
summarise.tbl_ts <- function(.data, ...) {
  cls <- class(.data)
  grped <- is.grouped_df(.data)
  if (grped) grps <- groups(.data)
  index <- get_index(.data)
  dots_cap <- quos(..., .named = TRUE)
  # Find the special formula from a set of quos
  sp_f <- tilde_detect(dots_cap)
  idx <- sp_f$index
  if (is_empty(idx)) { # if there's no ~ in ..., tbl_ts is dropped
    .data <- NextMethod()
    return(structure(.data, class = cls[-1])) # remove tbl_ts
  } else {
    str_time <- sp_f$var_name
    sym_time <- sym(str_time)
    fun <- sp_f$fun
    # check whether fun is in the dictionary
    if (is_false(fun %in% builtin_dict())) {
      abort(paste(fun, "is not supported yet."))
    }
    # using group_by, sometimes it drops class attributes, e.g. as.yearmon
    .data <- .data %>%
      ungroup() %>%
      dplyr::mutate(!!str_time := UQ(sym(fun))(!!index))
    sum_args <- dots_cap[-idx] # used for summarise
    if (grped) {
      .data <- .data %>%
        dplyr::group_by(!!!grps) %>%
        dplyr::group_by(!!sym_time, add = TRUE)
    } else {
      .data <- .data %>%
        dplyr::group_by(!!sym_time)
    }
    .data <- .data %>%
        dplyr::summarise(!!!sum_args)
    attr(.data, "key") <- if (grped) {
      # ToDo: check if grouping vars should be key variables
        map(grps, as_quosure)
      } else {
        key_vars()
      }
    attr(.data, "index") <- sym_time
    attr(.data, "interval") <- pull_interval(
      eval_tidy(sym_time, data = .data)
    )
    return(structure(.data, class = cls))
  }
}

#' @rdname summarise
#' @export
summarize <- summarise

tilde_detect <- function(...) { # x be a list of quosures
  dots_names <- names2(quos_auto_name(...))
  strs <- dots2str(...)
  sp_f <- grepl("^~", strs) # should only length(TRUE) <= 1
  sp_idx <- which(sp_f == TRUE, useNames = FALSE)
  sp_time <- gsub("^~(.*)\\()", "\\1", strs[sp_idx])
  return(list(
    index = sp_idx,
    fun = sp_time,
    var_name = dots_names[sp_idx]
  ))
}

builtin_dict <- function() {
  return(c(
    "year", "as.yearmon", "as.yearqtr", "as_date", "as.Date"
  ))
}

