## helper functions ------------------
min_na <- function(x) {
  min(x, na.rm = TRUE)
}

max_na <- function(x) {
  max(x, na.rm = TRUE)
}

is_identity <- function(x) { # x is quosure
  if (is_quosure(x)) {
    return(quo_get_expr(x) == 1)
  }
  "1" %in% as.character(purrr::map(x, quo_get_expr))
}

expand.grid2 <- function(...) {
  expand.grid(..., KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
}

is_constant <- function(x) {
  diff(range(x, na.rm = TRUE)) < .Machine$double.eps ^ 0.5
}

# Normalise the numerics to range from 0 to 1
normalise <- function(x, xmin = NULL, xmax = NULL) {
  if (is_constant(x)) return(x)
  if (is.null(xmin)) xmin <- min_na(x)
  if (is.null(xmax)) xmax <- max_na(x)
  (x - xmin) / (xmax - xmin)
}

min_diff <- function(x) {
  min(abs(diff(x, na.rm = TRUE)))
}
