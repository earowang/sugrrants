globalVariables("name")

#' Rearrange data into parent-children pair data for d3-hierarchy
#'
#' @param data A tabular data.
#' @param formula Node size on the lhs, nesting variables on the rhs.
#' @param root Character.
#' @param na.rm `TRUE` removes `NA` in the key.
#' @export
stratify <- function(data, formula, root, na.rm = FALSE) {
  lhs <- f_lhs(formula)
  rhs <- f_rhs(formula)
  if (is_null(lhs)) {
    size <- 1L
  }
  # ToDo: (1) a bare variable of the same length as the distinct key data in the `data`
  # (2) a global variable needs evaluating outside the `data`
  key <- parse_formula(rhs)
  key_data <- dplyr::distinct(data, !!! key) %>%
    dplyr::select(!!! key)
  nc <- ncol(key_data)
  lst_data <- vector("list", length = nc)
  if (nc > 1) { # nesting
    for (j in seq_len(nc - 1)) {
      tmp <- dplyr::tibble(
        name = key_data[[j]],
        parent = key_data[[j + 1]]
      )
      # ToDo: check if upper-level variable has fewer observations
      lst_data[[nc - j + 1]] <- dplyr::distinct(tmp) %>%
        mutate(size = ifelse(j == 1, size, NA))
    }
    uni_parent <- unique(tmp[["parent"]]) # last "tmp"
  } else { # crossing
    uni_parent <- key_data[[1]]
  }
  if (has_length(uni_parent, 1)) {
    lst_data[[1]] <- dplyr::tibble(name = uni_parent, parent = "") # "" equivalent to "null" in d3 data
  } else {
    if (is_missing(root)) {
      root <- deparse(substitute(data))
    }
    header <- dplyr::tibble(name = root, parent = "")
    if (nc > 1) {
      lst_data[[1]] <- header %>%
        dplyr::bind_rows(dplyr::tibble(name = uni_parent, parent = root))
    } else { # crossing
      lst_data[[1]] <- header %>%
        dplyr::bind_rows(
          dplyr::tibble(name = uni_parent, parent = root) %>% 
            mutate(size = size)
        )
    }
  }
  out <- dplyr::bind_rows(lst_data)
  if (na.rm) {
    na_idx <- which(is.na(out$name))
    out <- out[-na_idx, , drop = FALSE]
  }
  out
}

# interpret a nested calls A | B | C
parse_formula <- function(key) { # call
  # ToDo: symbols other than `|`, abort
  if (!is_bare_list(key) && has_length(key, 2) && key[[1]] != sym("-")) {
    return(parse_formula(key[[2]]))
  }
  if (is_bare_list(key, 2) || length(key) < 3)
    return(key)
  op <- key[[1]]
  x <- key[[2]]
  y <- key[[3]]
  if (op == sym("|")) {
    c(parse_formula(x), parse_formula(y))
  } else if (op == sym("-")) {
    c(parse_formula(x), expr(-parse_formula(y)))
  } else {
    key
  }
}
