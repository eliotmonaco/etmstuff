test_that("function output as expected", {
  expect_equal(calculate_age(
    as.Date(c("1980-01-01", "1988-01-17", "2019-10-25")),
    as.Date(c("2023-12-31", "2023-12-31", "2023-12-31"))
  ), c(16070, 13132, 1528) / 365.25)
})

test_that("args are not dates error", {
  expect_error(
    calculate_age(letters, LETTERS),
    "`date1` and `date2` must be formatted as dates"
  )
})

test_that("args not same length error", {
  expect_error(calculate_age(
    as.Date(c("1980-01-01", "1988-01-17", "2019-10-25")),
    as.Date(c("2023-12-31", "2023-12-31"))
  ), "`date1` and `date2` must be the same length")
})
