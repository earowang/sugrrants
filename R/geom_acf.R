#' @title Compute autocorrelation for temporal data
#'
#' @description Since the data input is \code{data.frame}, it's better to sort
#'  the date-times from early to recent and make implicit missing values explicit
#'  before using \code{geom_acfbar}.
#'  
#' @inheritParams ggplot2::geom_bar
#' @param lag.max An integer indicating the maximum lag at which to calculate the
#'    acf.
#' @param type A character string giving the type of the acf to be computed. The
#'    default is the "correlation" and other options are "covariance" and "partial".
#'
#' @author Earo Wang
#'
#' @examples
#'    library(dplyr)
#'    fstaff <- pedestrian %>%
#'      filter(Sensor_ID == 13)
#'    
#'    # use ggplot2 
#'    fstaff %>%
#'      ggplot(aes(x = ..lag.., y = Hourly_Counts)) +
#'      geom_acfbar()
#'
#' @export
#'
geom_acfbar <- function(mapping = NULL, data = NULL, 
  position = "identity", na.rm = FALSE, show.legend = NA, 
  inherit.aes = TRUE, lag.max = NULL, type = "correlation", ...) {
  layer(
    stat = StatAcf, geom = GeomAcfBar, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(lag.max = lag.max, type = type, na.rm = na.rm, ...)
  )
}

GeomAcfBar <- ggproto("GeomAcfBar", GeomBar,
  default_aes = c(aes(x = ..lag..), GeomBar$default_aes)
)
