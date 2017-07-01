filter.tbl_ts <- function(.data, ...) {
  key <- get_key(.data)
  index <- get_index(.data)
  interval <- get_interval(.data)
  cls <- class(.data)
  .data <- NextMethod()
  attrs <- list(key = key, index = index, interval = interval, class = cls)
  return(set_attrs(.data, splice(attrs)))
}

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
    attrs <- list(key = key, index = index, interval = interval, class = cls)
    return(set_attrs(.data, splice(attrs)))
  } else {
    return(set_attrs(.data, class = cls[-1]))
  }
}

# mutate behaviour for tbl_ts needs giving more thoughts
mutate.tbl_ts <- function(.data, ...) {
  key <- get_key(.data)
  index <- get_index(.data)
  interval <- get_interval(.data)
  cls <- class(.data)
  .data <- NextMethod()
  attrs <- list(key = key, index = index, interval = interval, class = cls)
  return(set_attrs(.data, splice(attrs)))
}

group_by.tbl_ts <- function(.data, ..., add = FALSE) {
  key <- get_key(.data)
  index <- get_index(.data)
  interval <- get_interval(.data)
  .data <- NextMethod(.Generic, object = .data, add = add)
  cls <- c("tbl_ts", class(.data))
  attrs <- list(key = key, index = index, interval = interval, class = cls)
  return(set_attrs(.data, splice(attrs)))
}

summarise.tbl_ts <- function(.data, ...) {
  cls <- class(.data)
  grped <- is.grouped_df(.data)
  index <- get_index(.data)
  dots_cap <- quos(..., .named = TRUE)
  sp_f <- tilde_detect(dots_cap)
  idx <- sp_f$index
  if (is_empty(idx)) {
    .data <- NextMethod()
    # have a smarter solution in dropping tbl_ts class?
    return(set_attrs(.data, class = cls[-1])) # remove tbl_ts
  } else {
    str_time <- sp_f$var_name
    sym_time <- as_quosure(sym(str_time))
    fun <- sp_f$fun
    # check whether fun is in the dictionary
    if (identical(fun %in% builtin_dict(), FALSE)) {
      abort(paste(fun, "is not supported yet."))
    }
    .data <- .data %>% 
      dplyr::mutate(!!str_time := UQ(sym(fun))(!!index)) # ToDo: lang
    sum_args <- dots_cap[-idx] # used for summarise
    .data <- .data %>% 
      dplyr::group_by(!!sym_time, add = grped) %>% 
      dplyr::summarise(!!!sum_args)
    attr(.data, "key") <- if (grped) {
        map(groups(.data), as_quosure)
      } else {
        as_quosure(key_vars())
      }
    attr(.data, "index") <- sym_time
    attr(.data, "interval") <- pull_interval(
      eval_tidy(sym_time, data = .data)
    )
    return(set_attrs(.data, class = cls))
  }
}

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
    "year", "as_date", "as.Date"
  ))
}

