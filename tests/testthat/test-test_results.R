test_that("check_results() output as expected 1", {
  df <- readRDS(test_path("data", "results.rds"))
  expect_equal(check_results(df$result), df$valid1)
})

test_that("clean_results() output as expected", {
  df <- readRDS(test_path("data", "results.rds"))
  expect_equal(clean_results(df$result), df$clean)
})

test_that("check_results() output as expected 2", {
  df <- readRDS(test_path("data", "results.rds"))
  expect_equal(check_results(df$clean), df$valid2)
})

test_that("parse_results() sign output as expected", {
  df <- readRDS(test_path("data", "results.rds"))
  expect_equal(parse_results(df$clean)$sign, df$result_sign)
})

test_that("parse_results() number output as expected", {
  df <- readRDS(test_path("data", "results.rds"))
  expect_equal(parse_results(df$clean)$number, df$result_number)
})
