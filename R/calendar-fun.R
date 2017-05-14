## Setting up different calendar layouts
setup_calendar <- function(x, ...) {
  UseMethod("setup_calendar")
}

setup_calendar.cal_monthly <- function(x, dir = "h", sunday = FALSE, 
  nrow = NULL, ncol = NULL, ...) {
  # x is a vector of unique dates
  date_x <- unique(as_date(x))
  dir <- match.arg(dir, choices = c("h", "v"))

  month_x <- unique(date_x - mday(date_x) + 1)
  nfacets <- length(month_x)
  nrow <- ceiling(nfacets / ncol) # overwrite user's row arg
  days_x <- days_in_month(month_x) # d
  ndays <- 7
  max_wks <- 5
  ncells <- max_wks * ndays
  if (sunday) { # Weekday starts with Sunday
    first_wday <- wday(month_x) # k
  } else { # starts with Monday
    first_wday <- wday2(month_x) # k
  }
  counter <- mapply2( # g
    function(x, y) x + 0:(y - 1), x = first_wday, y = days_x
  )
  row_idx <- lapply( # i
    counter, 
    function(x) ifelse(x == ncells, max_wks, ceiling((x %% ncells) / ndays))
  )
  col_idx <- lapply( # j
    counter, 
    function(x) ifelse(x %% ndays == 0, ndays, x %% ndays)
  )
  if (dir == "v") { # reverse col_idx and row_idx when direction is vertical
    col_tmp <- row_idx
    row_idx <- col_idx
    col_idx <- col_tmp
  }

  seq_facets <- seq_len(nfacets)
  m_idx <- ifelse(seq_facets %% ncol == 0, ncol, seq_facets %% ncol)
  last_rep <- m_idx[nfacets]
  n_idx <- rep(1:nrow, times = c(rep(ncol, nrow - 1), last_rep))
  if (dir == "h") {
    row_idx[] <- mapply2(
      function(x, y) x + max_wks * (y - 1), x = row_idx, y = n_idx
    )
    col_idx[] <- mapply2(
      function(x, y) x + ndays * (y - 1), x = col_idx, y = m_idx
    )
  } else { # dir = "v"
    row_idx[] <- mapply2(
      function(x, y) x + ndays * (y - 1), x = row_idx, y = n_idx
    )
    col_idx[] <- mapply2(
      function(x, y) x + max_wks * (y - 1), x = col_idx, y = m_idx
    )
  }
  cal_table <- data.frame(
    ROW = unlist2(row_idx),
    COL = unlist2(col_idx),
    MROW = rep(n_idx, days_x),
    MCOL = rep(m_idx, days_x),
    PANEL = seq_along(unlist2(counter)),
    MPANEL = rep(seq_len(nfacets), days_x)
  )
  return(cal_table)
}

