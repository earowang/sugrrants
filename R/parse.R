# ToDo: parse_key(key = key_vars((x * y) | z))
parse_key <- function(data, key = key_vars()) {
  key_exprs <- exprs(!!!get_expr(key))
  cn <- colnames(data)
  if (is_empty(key) || length(key) > 2) { # univariate || three or more vars
    # parse_key(key = key_vars())
    # parse_key(key = key_vars(x, y, z))
    return(structure(key_exprs, class = "key_ts"))
  } else {
    len_key <- length(key_exprs)
    syms_all <- c("|", "*")
    if (len_key == 2) {
      exprs_2 <- key_exprs[[2]]
      if (is_symbol(exprs_2)) {
        # parse_key(key = key_vars(x, y))
        return(structure(key_exprs, class = "key_ts"))
      } else if (exprs_2 == syms_all[2]) {
        # parse_key(key = key_vars(x:z, "*"))
        key2 <- select_vars(cn, !!key_exprs[[1]])
        key_lst <- c(sym(syms_all[2]), syms(key2))
        return(structure(key_lst, class = "key_gts"))
      } else {
        # parse_key(key = key_vars(-x, "|"))
        # parse_key(key = key_vars(y:z, "|"))
        # parse_key(key = key_vars(x:z, "|"))
        key2 <- select_vars(cn, !!key_exprs[[1]])
        key_lst <- c(sym(syms_all[1]), syms(key2))
        return(structure(key_lst, class = "key_hts"))
      }
    } else { # len_key == 1
      all_exprs <- all.vars(key_exprs[[1]], functions = TRUE)
      syms_has <- all_exprs[1]
      if (is_false(syms_has %in% syms_all)) {
        # parse_key(key = key_vars(x))
        # parse_key(key = key_vars(x:z))
        key2 <- syms(select_vars(cn, !!key_exprs[[1]]))
        return(structure(key2, class = "key_ts"))
      } else if (syms_has == syms_all[2]) {
        # parse_key(key = key_vars(x * y * z))
        key_lst <- map(all_exprs, sym)
        return(structure(key_lst, class = "key_gts"))
      } else {
        # parse_key(key = key_vars(x | y | z))
        key_lst <- map(all_exprs, sym)
        return(structure(key_lst, class = "key_hts"))
      }
    }
  }
}

