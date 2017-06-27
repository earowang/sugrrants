#' @title Compute autocorrelation for temporal data
#'
#' @description Since the data input is `data.frame`, it's better to sort
#'  the date-times from early to recent and make implicit missing values explicit
#'  before using `geom_acf`.
#'  
#' @inheritParams ggplot2::geom_bar
#' @param lag.max An integer indicating the maximum lag at which to calculate the
#'    acf.
#' @param type A character string giving the type of the acf to be computed. The
#' @param level A numeric defining the confidence level. If `NULL`, no significant
#'    line to be drawn.
#' @param na.rm Logical. If `TRUE`, missing values are removed.
#'    default is the "correlation" and other options are "covariance" and "partial".
#'
#' @author Earo Wang
#' @rdname sugrrants-geom
#'
#' @examples
#'    library(dplyr)
#'    fstaff <- pedestrian %>%
#'      filter(Sensor_ID == 13)
#'    
#'    # use ggplot2 
#'    fstaff %>%
#'      ggplot(aes(x = ..lag.., y = Hourly_Counts)) +
#'      geom_acf()
#'
#' @export
geom_acf <- function(mapping = NULL, data = NULL, 
  position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, 
  lag.max = NULL, type = "correlation", level = 0.95, ...) {
  layer(
    stat = StatAcf, geom = GeomAcf, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      lag.max = lag.max, type = type, level = level, 
      na.rm = na.rm, ...
    )
  )
}

#' @rdname sugrrants-geom
#' @format NULL
#' @usage NULL
#' @export
GeomAcf <- ggproto("GeomAcf", GeomBar,
  draw_panel = function(data, panel_scales, coord) {
    acf_hline <- transform(data, fill = NA, alpha = NA)
    acf_bar <- transform(data, colour = NA)

    has_hline <- !is.null(data$yintercept)

    gList(
      GeomBar$draw_panel(acf_bar, panel_scales, coord),
      if (has_hline) GeomHline$draw_panel(acf_hline, panel_scales, coord)
    )
  },

  draw_key = draw_key_acf,
  required_aes = "y",
  default_aes = c(
    aes(x = ..lag.., colour = "#3182bd", linetype = "dashed"), 
    GeomBar$default_aes
  )
)
