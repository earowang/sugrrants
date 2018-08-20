#' Rearrange data into parent-children pair data for d3-hierarchy
#'
#' @param data A tabular data.
#' @param ... Nesting variables.
#' @param size
#' * `1` means identity
#' * Bare variable name
#' * A vector of the same length as the distinct key
#' @param root Character.
#' @param na.rm `TRUE` removes `NA` in the key.
#' @export
stratify <- function(data, ..., size = 1L, root, na.rm = FALSE) {
  dots_expr <- enexprs(...)
  if (!has_length(dots_expr, 1)) {
    abort("`stratify()` only accepts one expression.")
  }
  dots_expr <- dots_expr[[1]]
  key <- parse_cond(dots_expr)
  key_data <- dplyr::distinct(data, !!! key) %>%
    dplyr::select(!!! key)
  nc <- ncol(key_data)
  lst_data <- vector("list", length = nc)
  if (nc > 1) { # nesting
    for (j in seq_len(nc - 1)) {
      tmp <- tibble::tibble(
        name = key_data[[j]],
        parent = key_data[[j + 1]]
      )
      # ToDo: check if upper-level variable has fewer observations
      lst_data[[nc - j + 1]] <- dplyr::distinct(tmp) %>%
        mutate(size = ifelse(j == 1, size, 0L))
    }
    uni_parent <- unique(tmp[["parent"]]) # last "tmp"
  } else { # crossing
    uni_parent <- key_data[[1]]
  }
  if (has_length(uni_parent, 1)) {
    lst_data[[1]] <- tibble::tibble(name = uni_parent, parent = "")
  } else {
    if (is_missing(root)) {
      root <- deparse(substitute(data))
    }
    lst_data[[1]] <- tibble::tibble(name = root, parent = "") %>%
      dplyr::bind_rows(tibble::tibble(name = uni_parent, parent = root))
  }
  out <- dplyr::bind_rows(lst_data)
  if (na.rm) {
    out <- tidyr::drop_na(out)
  }
  out
}

# interpret a nested calls A | B | C
parse_cond <- function(key) { # call
  if (!is_bare_list(key) && has_length(key, 2) && key[[1]] != sym("-")) {
    return(parse_cond(key[[2]]))
  }
  if (is_bare_list(key, 2) || length(key) < 3)
    return(key)
  op <- key[[1]]
  x <- key[[2]]
  y <- key[[3]]
  if (op == sym("|")) {
    c(parse_cond(x), parse_cond(y))
  } else if (op == sym("-")) {
    c(parse_cond(x), expr(-parse_cond(y)))
  } else {
    key
  }
}
