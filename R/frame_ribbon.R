globalVariables(c(".lu", ".rank"))

#' @title Get a data frame of long form ready for \code{ggplot2::geom_ribbon}
#'  
#' @description \code{ggplot2::geom_ribbon} requires at least two aesthetics mapping of 
#'    \code{ymin} and \code{ymax} to make a ribbon plot. \code{frame_ribbon} lubricates
#'    the transition from a long data form to the basic data form that is wanted for
#'    \code{ggplot2::geom_ribbon}. 
#'
#' @param .data A grouped data frame in a long form in conjunction with \code{group_by}.
#' @param .value A variable of numerics in order to produce the ribbon bounds.
#'
#' @return A data frame with new columns of \code{lower}, \code{upper}, possibly
#'    \code{middle} and \code{pair}.
#'
#' @details A ribbon plot can be seen as a simplified version of boxplot. However,
#'    the ribbon plot employs a smoother view if the continuous variable is used
#'    as one of the axes compared to the boxplot does for the categorical one.
#'
#' @author Earo Wang
#'
#' @examples
#'    # convert the summarised data frame to the ribbon format
#'    library(dplyr)
#'    ped_ribbon <- pedestrian %>%
#'      group_by(Sensor_Name, Sensor_ID) %>% 
#'      by_month(.index = Date_Time, .value = Hourly_Counts, .f = quantile) %>% 
#'      frame_ribbon(.value = .out)
#'    
#'    # use ggplot2 to produce the ribbon plot
#'    library(ggplot2)
#'      ggplot(ped_ribbon, aes(x = .month)) +
#'      geom_ribbon(aes(ymin = lower, ymax = upper, 
#'        alpha = .pair, colour = Sensor_Name)) +
#'      geom_line(aes(y = middle)) +
#'      facet_grid(Sensor_Name ~ ., scales = "free_y")
#'
#' @export
#'
frame_ribbon <- function(.data, .value) {
  if (!is.grouped_df(.data)) {
    stop(".data must be a grouped data frame", call. = FALSE)
  }
  frame_ribbon_(.data, f_capture(.value))
}

# frame_ribbon prepares a long data format for the use of geom_ribbon()
# ToDo: consider no grouping vars case
# SE
frame_ribbon_ <- function(.data, .value) {
  grouped_vars <- groups(.data)
  # tidy::spread_() doesn't support formula yet, convert to string first
  tidy_value <- deparse(as_name(.value))

  # set up basic info to find the pair of (lower, upper)
  len <- unique(group_size(.data))
  is_odd <- len %% 2 == 1
  mid_col <- ceiling(len / 2)
  seql <- seq_len(len)
  mid_seq <- seq_len(mid_col)
  # first half is needed, as the second half is redundant
  pairs_mat <- rbind(seql, rev(seql))[, mid_seq, drop = FALSE]
  pairs_lower <- pairs_mat[1, ]
  pairs_upper <- pairs_mat[2, ]
  if (is_odd) {
    pairs_lower <- pairs_lower[-mid_col]
    pairs_upper <- pairs_upper[-mid_col]
  }

  # row_number() gives the rank for each group in order to label lower & upper
  .data <- .data %>% 
    mutate_(.rank = f_interp(~ row_number(uq(.value)))) %>% 
    ungroup() %>% # case_when doesn't support group_by()
    mutate(.lu = case_when( 
      .$.rank %in% pairs_lower ~ "lower",
      .$.rank %in% pairs_upper ~ "upper",
      TRUE ~ "middle"
      )
    ) 
  if (is_odd) { # replicate middle column, which is not elegant
    .data_lu <- .data %>% 
      filter(.lu != "middle")
    pair_chr_lu <- character(nrow(.data_lu))
    for (i in mid_seq) {
      pair_chr_lu[.data_lu$.rank %in% pairs_mat[, i]] <- paste0("pair", i)
    }
    # long to wide as ymin = lower and ymax = upper in ggplot2
    .data_lu <- .data_lu %>% 
      select(-.rank) %>% 
      mutate(.pair = pair_chr_lu) %>% 
      spread_(".lu", tidy_value)

    # replicate middle column as many times as the number of the pairs
    .data_mid <- .data %>% 
      filter(.lu == "middle")
    rep_mid <- mid_col - 1
    .data_list <- vector(mode = "list", length = rep_mid)
    for (i in seq_len(rep_mid)) {
      .data_list[[i]] <- .data_mid %>% 
        mutate(.pair = paste0("pair", i))
    }
    .data_mid <- bind_rows(.data_list)
    .data_mid <- .data_mid %>% 
      select(-.rank) %>% 
      spread_(".lu", tidy_value)

    .data <- suppressMessages(left_join(.data_lu, .data_mid))
  } else {
    pair_chr <- character(nrow(.data))
    for (i in mid_seq) {
      pair_chr[.data$.rank %in% pairs_mat[, i]] <- paste0("pair", i)
    }
    .data <- .data %>% 
      select(-.rank) %>% 
      mutate(.pair = pair_chr) %>% 
      spread_(".lu", tidy_value)
  }
  return(.data)
}
