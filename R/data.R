#' Pedestrian counts in Melbourne city
#'
#' A dataset containing the pedestrian counts at hourly intervals from 2016-01-01
#' to 2017-04-20 at 7 sensors in the city of Melbourne. The variables are as follows:
#'
#' @format A tibble with 78755 rows and 9 variables:
#' \describe{
#'   \item{Date_Time}{Date time when the pedestrian counts are recorded}
#'   \item{Year}{Year associated with Date_Time}
#'   \item{Month}{Month associated with Date_Time}
#'   \item{Mdate}{Day of month associated with Date_Time}
#'   \item{Day}{Weekday associated with Date_Time}
#'   \item{Time}{Hour associated with Date_Time}
#'   \item{Sensor_ID}{Sensor identifiers}
#'   \item{Sensor_Name}{Sensor names}
#'   \item{Hourly_Counts}{Hourly pedestrian counts}
#' }
#' @docType data
#' @name pedestrian
#' @usage pedestrian
#' @examples
#'    pedestrian
"pedestrian"

#' Tidyverse core packages daily downloads
#'
#' A dataset containing the daily downloads of tidyverse core packages from 
#' 2015-01-01 to 2016-12-31. The variables are as follows:
#'
#' @format A data frame with 4386 rows and 3 variables:
#' \describe{
#'   \item{date}{Date when the downloads are made}
#'   \item{count}{Daily downloads}
#'   \item{package}{Tidyverse core package name}
#' }
#' @docType data
#' @name tidypkgs
#' @usage tidypkgs
#' @examples
#'    tidypkgs
"tidypkgs"
