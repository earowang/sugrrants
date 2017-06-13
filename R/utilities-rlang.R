possibly_string <- function(x) {
  check <- try(is_string(x), silent = TRUE)
  ifelse(class(check) != "try-error", TRUE, FALSE)
}

