#' Key drawing functions
#'
#' @inheritParams ggplot2::draw_key_point
#'
#' @return A grid grob
#'
#' @keywords internal
#' @name draw-key
NULL

#' @rdname draw-key
#' @export
draw_key_acf <- function(data, params, size) {
  grobTree(
    draw_key_rect(data, params),
    if (!is.null(params$level)) draw_key_path(data, params)
  )
}

