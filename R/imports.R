#' @importFrom lubridate as_date wday year days weeks mday month days_in_month
#' @importFrom lubridate isoweek month<-
#' @importFrom timeDate GoodFriday Easter
#' @importFrom dplyr group_by is.grouped_df groups ungroup group_size arrange
#' @importFrom dplyr if_else mutate select filter right_join summarise distinct
#' @importFrom dplyr left_join mutate_at ends_with vars funs
#' @importFrom tibble type_sum
#' @importFrom tidyr nest unnest
#' @importFrom purrr map map2
#' @importFrom ggplot2 resolution layer ggproto draw_key_rect draw_key_path 
#' @importFrom ggplot2 geom_label geom_text scale_x_continuous scale_y_continuous
#' @importFrom ggplot2 theme element_blank aes last_plot is.ggplot wrap_dims
#' @importFrom rlang eval_tidy quo enquo abort !! !!! := quo_name flatten_int
#' @importFrom rlang is_string syms
#' @importFrom magrittr %>%
#' @importFrom grid grobTree gList
NULL

#' @export
magrittr::`%>%`
