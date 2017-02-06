#' @title Compute autocorrelation for temporal data
#'
#' @description Since the data input is \code{data.frame}, it's better to sort
#'  the date-times from early to recent and make implicit missing values explicit
#'  before using \code{stat_acf}.
#'  
#' @inheritParams ggplot2::stat_identity
#' @param lag.max An integer indicating the maximum lag at which to calculate the
#'    acf.
#' @param type A character string giving the type of the acf to be computed. The
#' @param na.rm Logical. If \code{TRUE}, missing values are removed.
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
#'      stat_acf(geom = "bar")
#'
#' @export
#'
stat_acf <- function(mapping = NULL, data = NULL, geom = "bar",
  position = "identity", na.rm = FALSE, show.legend = NA, 
  inherit.aes = TRUE, lag.max = NULL, type = "correlation", ...) {
  layer(
    stat = StatAcf, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(lag.max = lag.max, type = type, na.rm = na.rm, ...)
  )
}

StatAcf <- ggproto("StatAcf", Stat,
  required_aes = "y",
  default_aes = aes(x = ..lag..),

  compute_group = function(data, scales, params, 
    lag.max = NULL, type = "correlation") {
    acf_y <- acf(data$y, lag.max = lag.max, type = type, plot = FALSE)
    acf_df <- data.frame(lag = acf_y$lag[-1, , 1], y = acf_y$acf[-1, , 1])
    return(acf_df)
  }
)
