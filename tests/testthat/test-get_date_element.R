test_that("output as expected", {
  expect_equal(
    get_year(c("2023-7-5", "2022-06-08", "20241229")),
    c(2022, 2023)
  )
})

test_that("output as expected", {
  expect_equal(
    get_year(c("2023-7-5", "2022-06-08", "20241229"), format = "%Y%m%d"),
    2024
  )
})

test_that("output as expected", {
  expect_equal(
    get_quarter(c("2023-7-5", "2023-06-08", "20231229")),
    c(2, 3)
  )
})

test_that("output as expected", {
  expect_equal(
    get_quarter(c("2023-7-5", "2023-06-08", "20231229"), format = "%Y%m%d"),
    4
  )
})

test_that("output as expected", {
  expect_message(
    get_quarter(c("2022-7-5", "2023-06-08", "20231229")),
    "`dates` represents multiple years"
  )
})
