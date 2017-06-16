possibly_string <- function(x) {
  check <- try(is_string(x), silent = TRUE)
  ifelse(class(check) != "try-error", TRUE, FALSE)
}

possibly_identity <- function(x) { # x can be a list or a vector
  any(vapply(x, function(x) x == 1, logical(1)))
}
