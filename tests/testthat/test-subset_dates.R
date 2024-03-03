test_that("range1 is year", {
  df <- readRDS(test_path("data", "dates1.rds"))
  expect_equal(
    subset_dates(df, var = "date", range1 = "2021", silent = TRUE),
    tibble::tibble(date = seq.Date(as.Date("2021-01-01"), as.Date("2021-12-31"), by = "day"))
  )
})

test_that("range1 and range2 are years", {
  df <- readRDS(test_path("data", "dates1.rds"))
  expect_equal(
    subset_dates(df, var = "date", range1 = "2021", range2 = "2025", silent = TRUE),
    tibble::tibble(date = seq.Date(as.Date("2021-01-01"), as.Date("2022-12-31"), by = "day"))
  )
})

test_that("range1 is year-quarter", {
  df <- readRDS(test_path("data", "dates1.rds"))
  expect_equal(
    subset_dates(df, var = "date", range1 = "2022q3", silent = TRUE),
    tibble::tibble(date = seq.Date(as.Date("2022-07-01"), as.Date("2022-09-30"), by = "day"))
  )
})

test_that("range1 and range2 are year-quarters", {
  df <- readRDS(test_path("data", "dates1.rds"))
  expect_equal(
    subset_dates(df, var = "date", range1 = "2021q4", range2 = "2022q1", silent = TRUE),
    tibble::tibble(date = seq.Date(as.Date("2021-10-01"), as.Date("2022-03-31"), by = "day"))
  )
})

test_that("range2 quarter is 4", {
  df <- readRDS(test_path("data", "dates1.rds"))
  expect_equal(
    subset_dates(df, var = "date", range1 = "2020q4", range2 = "2021q4", silent = TRUE),
    tibble::tibble(date = seq.Date(as.Date("2020-10-01"), as.Date("2021-12-31"), by = "day"))
  )
})

test_that("range1 is year, range2 is year-quarter", {
  df <- readRDS(test_path("data", "dates1.rds"))
  expect_equal(
    subset_dates(df, var = "date", range1 = "2021", range2 = "2021q3", silent = TRUE),
    tibble::tibble(date = seq.Date(as.Date("2021-01-01"), as.Date("2021-09-30"), by = "day"))
  )
})

test_that("range1 is date", {
  df <- readRDS(test_path("data", "dates1.rds"))
  expect_equal(
    subset_dates(df, var = "date", range1 = "2022-09-12", silent = TRUE),
    tibble::tibble(date = as.Date("2022-09-12"))
  )
})


test_that("range1 and range2 are dates", {
  df <- readRDS(test_path("data", "dates1.rds"))
  expect_equal(
    subset_dates(df, var = "date", range1 = "2022-01-17", range2 = "2022-10-25", silent = TRUE),
    tibble::tibble(date = seq.Date(as.Date("2022-01-17"), as.Date("2022-10-25"), by = "day"))
  )
})


test_that("range2 precedes range1", {
  df <- readRDS(test_path("data", "dates1.rds"))
  expect_equal(
    subset_dates(df, var = "date", range1 = "2022-12-31", range2 = "2022-12-01", silent = TRUE),
    tibble::tibble(date = as.Date(""))[0,]
  )
})

test_that("error: `var` is numeric", {
  df <- readRDS(test_path("data", "dates2.rds"))
  expect_error(
    subset_dates(df, var = "number", range1 = "2021", silent = TRUE),
    "`df$var` must be formatted as a date",
    fixed = TRUE
  )
})

test_that("error: `var` is character", {
  df <- readRDS(test_path("data", "dates2.rds"))
  expect_error(
    subset_dates(df, var = "character", range1 = "2021", silent = TRUE),
    "`df$var` must be formatted as a date",
    fixed = TRUE
  )
})
