#' @rdname frame-calendar
#' @param plot A "ggplot" object or "plotly".
#' @param label If "label" is specified, it will add month/week text on the
#' `ggplot` object, which is actually passed to `geom_label()`. If "text" is
#' specified, it will add weekday/day of month text on the `ggplot` object,
#' which is actually passed to `geom_text()`. By default, both "label" and
#' "text" are used. If "text2" is specified for the "monthly" calendar only,
#' it will add day of month to the `ggplot` object.
#' @param locale ISO 639 language code. The default is "en" (i.e. US English).
#' For other languages support, package **readr** needs to be installed. 
#' See [readr::locale] for more details.
#' @param abbr Logical to specify if the abbreviated version of label should be
#' used.
#' @param ... Extra arguments passed to `geom_label()` and `geom_text()`
#' @export
prettify <- function(plot, label = c("label", "text"), locale, abbr = TRUE,
  ...) {
  if (missing(plot)) {
    plot <- last_plot()
  }
  if (is.null(label)) {
    label_arg <- NULL
  } else {
    label_arg <- match.arg(
      label, c("label", "text", "text2"),
      several.ok = TRUE
    )
  }
  UseMethod("prettify")
}

#' @export
prettify.plotly <- function(plot, label = c("label", "text"), locale, abbr = TRUE,
  ...) {
  a <- list(
    title = "",
    zeroline = FALSE,
    autotick = FALSE,
    showticklabels = FALSE,
    showline = FALSE,
    showgrid = FALSE
  )
  ly_data <- plotly::plotly_data(plot)
  label <- get_label(ly_data)
  text <- get_text(ly_data)
  cal <- get_calendar(ly_data)
  lst <- pre_plot(cal, label, text, locale, abbr)
  label <- lst$label
  text <- lst$text
  if ("label" %in% label_arg) {
    plot <- plot %>% 
      plotly::add_text(x = ~ x, y = ~ y, text = ~ label, data = label, ...)
  }
  if ("text" %in% label_arg) {
    plot <- plot %>% 
      plotly::add_text(x = ~ x, y = ~ y - 0.03, text = ~ label, data = text, ...)
  }
  if ("text2" %in% label_arg) {
    warn("`label = 'text2'` is ignored for plotly.")
  }
  plotly::layout(plot, showlegend = FALSE, xaxis = a, yaxis = a)
}

#' @export
prettify.ggplot <- function(plot, label = c("label", "text"), locale, abbr = TRUE,
  ...) {
  if (!("tbl_cal" %in% class(plot$data))) {
    abort("`prettify()` does not know how to handle with this type of data.")
  }
  label <- get_label(plot$data)
  text <- get_text(plot$data)
  cal <- get_calendar(plot$data)
  lst <- pre_plot(cal, label, text, locale, abbr)
  label <- lst$label
  text <- lst$text
  breaks <- get_breaks(plot$data)
  minor_breaks <- get_minor_breaks(plot$data)
  dir <- get_dir(plot$data)
  mar <- get_margin(plot$data)

  # separate params for geom_label and geom_text from ...
  param_list <- list(...)
  if (has_length(param_list, 0)) {
    label_param <- text_param <- text2_param <- list()
  } else {
    names_param <- names(param_list)
    label_all <- c(GeomLabel$aesthetics(), GeomLabel$parameters(TRUE))
    text_all <- c(GeomText$aesthetics(), GeomText$parameters(TRUE))
    label_param <- param_list[which(names_param %in% label_all)]
    text_param <- text2_param <- param_list[which(names_param %in% text_all)]
  }

  if ("label" %in% label_arg) {
    label_param$data <- label
    label_param$mapping <- aes(x, y, label = label)
    label_param$hjust <- label_param$vjust <- 0
    label_param$inherit.aes <- FALSE
    plot <- plot +
      do.call(geom_label, label_param)
  }
  half_y <- (mar[2] / 2)
  if ("text" %in% label_arg) {
    text_param$data <- text
    text_param$mapping <- aes(x, y, label = label)
    text_param$inherit.aes <- FALSE
    if (dir == "h") {
      text_param$nudge_y <- - half_y / 2
      text_param$vjust <- 1
      plot <- plot +
        do.call(geom_text, text_param)
    } else {
      text_param$nudge_x <- - (mar[1] / 2)
      text_param$hjust <- 1
      plot <- plot +
        do.call(geom_text, text_param)
    }
  }
  if ("text2" %in% label_arg) {
    text2 <- get_text2(plot$data)
    if (is.null(text2)) {
      warn("`label = 'text2'` is ignored for this type of calendar.")
    } else {
      text2_param$data <- text2
      text2_param$mapping <- aes(x, y, label = label)
      text2_param$inherit.aes <- FALSE
      text2_param$nudge_y <- -0.01
      text2_param$hjust <- 0
      text2_param$vjust <- 1
      plot <- plot +
        do.call(geom_text, text2_param)
    }
  }
  plot <- plot +
    scale_x_continuous(breaks = breaks$x, minor_breaks = minor_breaks$x)
  plot <- plot +
    scale_y_continuous(breaks = breaks$y, minor_breaks = minor_breaks$y)
  plot <- plot +
    expand_limits(y = c(min_na(breaks$y) - half_y, max_na(breaks$y) + half_y)) +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank()
    )
  plot
}

# helper functions for frame_calendar
get_margin <- function(data) {
  attr(data, "margin")
}

get_breaks <- function(data) {
  attr(data, "breaks")
}

get_minor_breaks <- function(data) {
  attr(data, "minor_breaks")
}

get_label <- function(data) {
  attr(data, "label")
}

get_text <- function(data) {
  attr(data, "text")
}

get_text2 <- function(data) {
  attr(data, "text2")
}

get_dir <- function(data) {
  attr(data, "dir")
}

get_calendar <- function(data) {
  attr(data, "calendar")
}

gen_wday_index <- function(sunday = FALSE) {
  if (sunday) 1:7 else c(2:7, 1)
}

gen_day_breaks <- function(grids) {
  # day breaks
  minor_xbreaks_df <- grids %>%
    group_by(COL) %>%
    summarise(
      .xminor_min = min(.gx)
    )
  minor_xbreaks <- minor_xbreaks_df$.xminor_min
  minor_ybreaks_df <- grids %>%
    group_by(ROW) %>%
    summarise(.yminor_min = min(.gy))
  minor_ybreaks <- minor_ybreaks_df$.yminor_min
  list(x = minor_xbreaks, y = minor_ybreaks)
}

pre_plot <- function(calendar, label, text, locale, abbr = TRUE) {
  if (missing(locale)) {
    locale <- "en"
  }
  if (locale != "en") {
    if (!requireNamespace("readr", quietly = TRUE)) {
      stop(
        "Package 'readr' required for other languages support", ".\n",
        "Please install and try again.", call. = FALSE
      )
    }
    loc_dn <- readr::locale(date_names = locale)$date_names
  } else {
    loc_dn <- list(
      mon_ab = month.abb,
      day_ab = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
      mon = month.name,
      day = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday" )
    )
  }
  if (abbr) {
    mtext <- loc_dn$mon_ab
    dtext <- loc_dn$day_ab
    # a single letter
    if (locale == "en") dtext <- substring(dtext, first = 1, last = 1)
  } else {
    mtext <- loc_dn$mon
    dtext <- loc_dn$day
  }
  if (calendar == "monthly") {
    nyr <- unique.default(label$year)
    seq_label <- mtext[label$mon]
    if (length(nyr) > 2) seq_label <- paste0(label$year, seq_label)
    label <- bind_cols(label, label = seq_label)
    text <- bind_cols(text, label = dtext[text$day])
  } else if (calendar == "weekly") {
    text <- bind_cols(text, label = dtext[text$day])
  } else if (calendar == "daily") {
    seq_label <- mtext[label$mon]
    label <- bind_cols(label, label = seq_label)
  }
  list(label = label, text = text)
}
