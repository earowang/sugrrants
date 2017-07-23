library(sugrrants)
context("Test au_holiday()")

test_that("The argument year is double/integer", {
  expect_error(au_holiday(year = "2010"))
})

test_that("The output", {
  vic2000 <- au_holiday(year = 2000)
  expect_match(vic2000[vic2000$date == as.Date("2000-01-26"), "holiday"], "Australia Day")
  expect_match(vic2000[vic2000$date == as.Date("2000-04-21"), "holiday"], "Good Friday")
  expect_match(vic2000[vic2000$date == as.Date("2000-11-07"), "holiday"], "Melbourne Cup")

  vic0016 <- au_holiday(year = 2000:2016)
  expect_equal(dim(vic0016), c(17 * 11, 2))
})
