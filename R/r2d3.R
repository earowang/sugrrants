#' Plot D3 hierarchy
#'
#' @param data A parent-name paired data frame
#' @inheritParams r2d3::r2d3
#' @rdname r2d3
#' @export
d3_sunburst <- function(data, width = NULL, height = NULL) {
  r2d3::r2d3(
    data = data,
    d3_version = 4,
    script = system.file("d3/sunburst/sunburst.js", package = "sugrrants"),
    css = system.file("d3/sunburst/sunburst.css", package = "sugrrants"),
    width = width,
    height = height
  )
}

#' @rdname r2d3
#' @export
d3_indented_tree <- function(data, width = NULL, height = NULL) {
  r2d3::r2d3(
    data = data,
    d3_version = 4,
    script = system.file("d3/indented-tree/indented-tree.js", package = "sugrrants"),
    css = system.file("d3/indented-tree/indented-tree.css", package = "sugrrants"),
    width = width,
    height = height
  )
}

#' @rdname r2d3
#' @export
d3_treemap <- function(data, width = NULL, height = NULL) {
  r2d3::r2d3(
    data = data,
    d3_version = 4,
    script = system.file("d3/treemap/treemap.js", package = "sugrrants"),
    css = system.file("d3/treemap/treemap.css", package = "sugrrants"),
    width = width,
    height = height
  )
}
#' @rdname r2d3
#' @export
d3_indented_tree <- function(data, width = NULL, height = NULL) {
  r2d3::r2d3(
    data = data,
    d3_version = 4,
    script = system.file("d3/indented-tree/indented-tree.js", package = "sugrrants"),
    css = system.file("d3/indented-tree/indented-tree.css", package = "sugrrants"),
    width = width,
    height = height
  )
}
