library(dplyr)
library(sugrrants)
context("Test frame_calendar()")

test_that("Multiple y's and NA's", {
  set.seed(1234)
  dat <- data.frame(
    Date = rep(seq(as.Date("2017-01-01"), as.Date("2017-03-31"), by = 1), 100),
    x = rnorm(9000), ymin = rnorm(9000), ymax = rnorm(9000, mean = 4)
  )
  cal_dat <- dat %>% 
    frame_calendar(x = x, y = vars(ymin, ymax), date = Date)
  cn_cal <- colnames(cal_dat)
  expect_true(all(c(".x", ".ymin", ".ymax") %in% cn_cal))
  dat_na <- dat
  dat_na[sample(1:9000, 100), ] <- NA
  cal_dat_na <- dat_na %>% 
    frame_calendar(x = x, y = vars(ymin, ymax), date = Date)
  expect_equal(dim(cal_dat), dim(cal_dat_na))
})

test_that("The argument date is Date class", {
  expect_error(
    frame_calendar(pedestrian, x = Time, y = Hourly_Counts, date = Date_Time)
  )
})

test_that("Some column names of data are used in the function", {
  ped <- rename(pedestrian, .gx = Date_Time)
  expect_error(
    frame_calendar(ped, x = Time, y = Hourly_Counts, date = Date)
  )
  ped <- rename(pedestrian, .cx = Date_Time)
  expect_error(
    frame_calendar(ped, x = Time, y = Hourly_Counts, date = Date)
  )
  ped <- rename(pedestrian, .ymax = Date_Time)
  expect_error(
    frame_calendar(ped, x = Time, y = Hourly_Counts, date = Date)
  )
  ped <- rename(pedestrian, theta = Date_Time)
  expect_error(
    frame_calendar(ped, x = Time, y = Hourly_Counts, date = Date, polar = TRUE)
  )
  ped <- rename(pedestrian, theta = Date_Time)
  expect_is(
    frame_calendar(ped, x = Time, y = Hourly_Counts, date = Date), "ggcalendar"
  )
  ped <- rename(pedestrian, .day = Date_Time)
  expect_error(
    frame_calendar(
      ped, x = Time, y = Hourly_Counts, date = Date, scale = "free_wday"
    )
  )
  expect_error(
    frame_calendar(
      ped, x = Time, y = Hourly_Counts, date = Date, scale = "free_mday"
    )
  )
  expect_is(
    frame_calendar(
      ped, x = Time, y = Hourly_Counts, date = Date, scale = "free"
    ),
    "ggcalendar"
  )
})

test_that("The argument calendar", {
  mcal <- frame_calendar(
    pedestrian, x = Time, y = Hourly_Counts, date = Date, calendar = "monthly"
  )
  expect_match(sugrrants:::get_calendar(mcal), "monthly")
  wcal <- frame_calendar(
    pedestrian, x = Time, y = Hourly_Counts, date = Date, calendar = "weekly"
  )
  expect_match(sugrrants:::get_calendar(wcal), "weekly")
  dcal <- frame_calendar(
    pedestrian, x = Time, y = Hourly_Counts, date = Date, calendar = "daily"
  )
  expect_match(sugrrants:::get_calendar(dcal), "daily")
})

test_that("The arguments x, y, date are unquoted variables", {
  expect_error(
    frame_calendar(
      pedestrian, x = "Time", y = Hourly_Counts, date = Date
    )
  )
  expect_error(
    frame_calendar(
      pedestrian, x = Time, y = "Hourly_Counts", date = Date
    )
  )
  expect_error(
    frame_calendar(
      pedestrian, x = Time, y = Hourly_Counts, date = "Date"
    )
  )
})

test_that("The grouped data", {
  grp_cal <- pedestrian %>% 
    group_by(Sensor_ID) %>% 
    frame_calendar(x = Time, y = Hourly_Counts, date = Date)
  expect_is(grp_cal, "ggcalendar")
  expect_is(grp_cal, "grouped_df")
})

test_that("The arguments width & heigth", {
  expect_error(
    frame_calendar(
      pedestrian, x = Time, y = Hourly_Counts, date = Date, width = 2
    )
  )
  expect_error(
    frame_calendar(
      pedestrian, x = Time, y = Hourly_Counts, date = Date, height = 2
    )
  )
})

test_that("The argument sunday", {
  expect_message(
    frame_calendar(
      pedestrian, x = Time, y = Hourly_Counts, date = Date, calendar = "weekly",
      sunday = TRUE
    )
  )
  expect_message(
    frame_calendar(
      pedestrian, x = Time, y = Hourly_Counts, date = Date, calendar = "daily",
      sunday = TRUE
    )
  )
  expect_is(
    frame_calendar(
      pedestrian, x = Time, y = Hourly_Counts, date = Date, sunday = TRUE
    ),
    "ggcalendar"
  )
})

test_that("The arguments nrow & ncol", {
  expect_message(
    frame_calendar(
      pedestrian, x = Time, y = Hourly_Counts, date = Date, calendar = "daily",
      ncol = 4
    )
  )
  expect_message(
    frame_calendar(
      pedestrian, x = Time, y = Hourly_Counts, date = Date, calendar = "weekly",
      nrow = 4
    )
  )
})

test_that("The identity 1", {
  cal <- frame_calendar(pedestrian, x = 1, y = Hourly_Counts, date = Date)
  cn_cal <- colnames(cal)
  expect_true(".x" %in% cn_cal)

  cal <- frame_calendar(pedestrian, x = Time, y = 1, date = Date)
  cn_cal <- colnames(cal)
  expect_true(".y" %in% cn_cal)

  cal <- frame_calendar(pedestrian, x = 1, y = 1, date = Date)
  cn_cal <- colnames(cal)
  expect_true(all(c(".x", ".y") %in% cn_cal))

  ped <- rename(pedestrian, .x = Date_Time)
  expect_error(
    frame_calendar(ped, x = 1, y = Hourly_Counts, date = Date)
  )

  cal <- frame_calendar(ped, x = Time, y = 1, date = Date)
  cn_cal <- colnames(cal)
  expect_true(".y" %in% cn_cal)
})

test_that("The argument dir", {
  hcal <- frame_calendar(pedestrian, x = Time, y = Hourly_Counts, date = Date)
  vcal <- frame_calendar(pedestrian, x = Time, y = Hourly_Counts, date = Date,
    dir = "v")
  expect_equal(dim(hcal), dim(vcal))
})

test_that("The argument polar", {
  ccal <- frame_calendar(pedestrian, x = Time, y = Hourly_Counts, date = Date)
  pcal <- frame_calendar(pedestrian, x = Time, y = Hourly_Counts, date = Date,
    polar = TRUE)
  expect_equal(dim(ccal), dim(pcal))
})

test_that("The output", {
  cal <- frame_calendar(pedestrian, x = Time, y = Hourly_Counts, date = Date)
  expect_is(cal, c("ggcalendar", "tbl_df"))
  expect_equal(nrow(cal), nrow(pedestrian))
  expect_equal(ncol(cal), ncol(pedestrian) + 2)

  pedestrian2 <- sample_frac(pedestrian)
  cal2 <- frame_calendar(pedestrian2, x = Time, y = Hourly_Counts, date = Date)
  expect_equal(cal, cal2)
})
