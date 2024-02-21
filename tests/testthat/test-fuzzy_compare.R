test_that("output as expected", {
  df1 <- readRDS(test_path("data", "addresses1.rds"))
  df2 <- readRDS(test_path("data", "addresses2.rds"))
  df_results <- readRDS(test_path("data", "addresses_fz.rds"))
  expect_equal(
    fuzzy_compare(
      df1, df2, row_id = "id",
      fuzzy_var = c("street", "unit"),
      exact_var = c("city", "state", "zip")
    ),
    df_results
  )
})
