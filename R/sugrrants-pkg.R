#' sugrrants: supporting graphs for analysing time series
#'
#' Provides 'ggplot2' graphics for analysing time series data. It aims to fit 
#' into the 'tidyverse' and grammar of graphics framework for handling temporal 
#' data.
#'
#' @aliases NULL sugrrants-package
#' @importFrom lubridate as_date wday year days weeks mday month days_in_month
#' @importFrom lubridate isoweek month<-
#' @importFrom readr locale
#' @importFrom dplyr group_by is.grouped_df ungroup arrange group_size groups
#' @importFrom dplyr if_else mutate select filter right_join summarise distinct
#' @importFrom dplyr left_join mutate_at ends_with vars funs between bind_cols
#' @importFrom dplyr %>%
#' @importFrom tidyr nest unnest gather
#' @importFrom ggplot2 resolution layer ggproto draw_key_rect draw_key_path 
#' @importFrom ggplot2 geom_label geom_text scale_x_continuous scale_y_continuous
#' @importFrom ggplot2 theme element_blank aes last_plot is.ggplot wrap_dims
#' @importFrom ggplot2 GeomLabel GeomText
#' @importFrom grid grobTree gList
#' @import rlang
"_PACKAGE"

