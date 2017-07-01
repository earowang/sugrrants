#' Create a tsibble object
#'
#' @param ... A set of name-value pairs.
#' @param key Unquoted variable(s) indicating the key variables for tsibble, 
#'    used in combination with `key_vars()`.
#' @param index An unquoted variable indicating the time index variable
#'
#' @return A tsibble object.
#' @author Earo Wang
#' @rdname tsibble
#' @seealso [tibble::tibble]
#'
#' @examples
#'    ts_df <- tsibble(
#'      Date = rep(seq(as.Date("2017-01-01"), by = 1, length = 10), 2),
#'      Group = rep(c("A", "B"), each = 10),
#'      Value = rnorm(20),
#'      key = key_vars(Group), index = Date
#'    ) 
#'    print(ts_df)
#'
#' @export
tsibble <- function(..., key = key_vars(), index) {
  index <- enquo(index)
  tsibble_(..., key = key, index = index)
}

## tsibble is a special class of tibble that handles with temporal data. It
## requires a sequence of time index to be unique across every identifier.
## The way to distinguish univariate or multivariate series is based on "key".
## Although the "index" arg is possible to automate the detection of time
## objects, it would fail when tsibble contain multiple time objects. Better
## to let user specify.
tsibble_ <- function(..., key = key_vars(), index) {
  tbl <- as_tibble(tibble::lst(...))
  cls_tbl <- class(tbl)
  if (is_empty(key)) { # if key = key_vars(), univariate time series
    eval_idx <- eval_tidy(index, data = tbl)
    if (anyDuplicated(eval_idx) != 0) {
      abort("'index' should be unique time objects across each key variable.")
    }
    tbl_interval <- pull_interval(eval_idx)
  } else { # otherwise multivariate time series
    tbl_nest <- tbl %>%
      group_by(!!!key) %>%
      nest()
    eval_lst_idx <- tbl_nest$data %>%
      map(function(data) eval_tidy(index, data = data))
    lst_interval <- vapply(eval_lst_idx, 
      function(x) gen_interval(x), numeric(1))
    if (!is_constant(lst_interval)) {
      abort("Each key variable must have the same time interval in 'tsibble'.")
    } else {
      tbl_interval <- pull_interval(eval_lst_idx[[1]])
    }
  }
  attr(tbl, "key") <- key
  attr(tbl, "index") <- index
  attr(tbl, "interval") <- tbl_interval
 output <- set_attrs(tbl, class = c("tbl_ts", cls_tbl))
}

get_key <- function(tbl_ts) {
  attr(tbl_ts, "key")
}

get_interval <- function(tbl_ts) {
  attr(tbl_ts, "interval")
}

get_index <- function(tbl_ts) {
  attr(tbl_ts, "index")
}

#' @param x A tsibble object.
#' @param ... Extra arguments.
#' @rdname tsibble
#' @export
print.tbl_ts <- function(x, ...) {
  int_x <- get_interval(x)
  grp_var <- get_key(x)
  if (is_empty(grp_var)) {
    cat("# A tsibble of", int_x$display(), "time interval", "\n")
  } else {
    cat(
      "# A tsibble of", int_x$display(), "time interval", "for", 
      cat_chr(x, grp_var), "\n"
    )
  }
  NextMethod()
}

#' @param ... Unquoted variable(s).
#' @rdname tsibble
#' @keywords internal
#' @export
key_vars <- function(...) {
  return(quos(...))
}

cat_chr <- function(.data, ...) {
  UseMethod("cat_chr")
}

cat_chr.tbl_ts <- function(.data, ...) { # ... is quos
  paste(dots2str(...), collapse = ", ")
}

