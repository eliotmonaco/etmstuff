test_that("output as expected", {
  df <- readRDS(test_path("data", "results.rds"))
  expect_equal(check_results(df$result), df$valid1)
})

test_that("output as expected", {
  df <- readRDS(test_path("data", "results.rds"))
  expect_equal(clean_results(df$result), df$clean)
})

test_that("output as expected", {
  df <- readRDS(test_path("data", "results.rds"))
  expect_equal(check_results(df$clean), df$valid2)
})

test_that("output as expected", {
  df <- readRDS(test_path("data", "results.rds"))
  expect_equal(parse_results(df$clean)$sign, df$result_sign)
})

test_that("output as expected", {
  df <- readRDS(test_path("data", "results.rds"))
  expect_equal(parse_results(df$clean)$number, df$result_number)
})

test_that("output as expected", {
  df <- readRDS(test_path("data", "results.rds"))
  expect_equal(flag_results(df$result_sign, df$result_number), df$flag_result)
})
