#' @title Compute autocorrelation for temporal data
#'
#' @description Since the data input is `data.frame`, it's better to sort
#'  the date-times from early to recent and make implicit missing values explicit
#'  before using `stat_acf`.
#'  
#' @inheritParams ggplot2::stat_identity
#' @param lag.max An integer indicating the maximum lag at which to calculate the
#'    acf.
#' @param type A character string giving the type of the acf to be computed. The
#'    default is the "correlation" and other options are "covariance" and "partial".
#' @param level A numeric defining the confidence level. If `NULL`, no significant
#'    line to be drawn.
#' @param na.rm Logical. If `TRUE`, missing values are removed.
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
  inherit.aes = TRUE, lag.max = NULL, type = "correlation", level = 0.95, ...) {
  layer(
    stat = StatAcf, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(lag.max = lag.max, type = type, level = level,
      na.rm = na.rm, ...)
  )
}

StatAcf <- ggproto("StatAcf", Stat,
  required_aes = "y",
  default_aes = aes(x = ..lag..),

  compute_group = function(data, scales, params, 
    lag.max = NULL, type = "correlation", level = 0.95) {

    acf_y <- acf(data$y, lag.max = lag.max, type = type, plot = FALSE)
    acf_df <- data.frame(lag = acf_y$lag[-1, , 1], y = acf_y$acf[-1, , 1])

    nr_acf <- nrow(acf_df)
    n_upper <- ceiling(nr_acf / 2)
    n_lower <- nr_acf - n_upper

    if (!is.null(level)) {
      ci <- qnorm((1 + level) / 2) / sqrt(acf_y$n.used)
      ci2 <- c(-ci, ci)
      acf_df <- transform(acf_df, yintercept = rep(ci2, c(n_upper, n_lower)))
    }
    return(acf_df)
  }
)
