test_that("output as expected", {
  expect_equal(
    check_date_seq(
      as.Date(c("1980-08-01", "1988-01-17", "2019-10-25")),
      as.Date(c("1980-12-31", "1988-01-17", "2000-12-31")),
      as.Date(c("2980-12-31", "1988-01-17", "2020-12-31"))
    ),
    c(TRUE, TRUE, FALSE)
  )
})
