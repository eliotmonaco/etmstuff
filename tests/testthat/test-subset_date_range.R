test_that("output as expected", {
  expect_equal(
    subset_date_range(
      dplyr::tibble(date = seq.Date(as.Date("2020-01-01"), as.Date("2022-12-31"), by = "day")),
      var = "date", range = "2021"
    ),
    dplyr::tibble(date = seq.Date(as.Date("2021-01-01"), as.Date("2021-12-31"), by = "day"))
  )
})

test_that("output as expected", {
  expect_equal(
    subset_date_range(
      dplyr::tibble(date = seq.Date(as.Date("2020-01-01"), as.Date("2022-12-31"), by = "day")),
      var = "date", range = "2020q3"
    ),
    dplyr::tibble(date = seq.Date(as.Date("2020-07-01"), as.Date("2020-09-30"), by = "day"))
  )
})

test_that("output as expected", {
  expect_equal(
    subset_date_range(
      dplyr::tibble(date = seq.Date(as.Date("2020-01-01"), as.Date("2022-12-31"), by = "day")),
      var = "date", range = c("2021-12-03", "2022-02-22")
    ),
    dplyr::tibble(date = seq.Date(as.Date("2021-12-03"), as.Date("2022-02-22"), by = "day"))
  )
})
