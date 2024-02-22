test_that("output as expected", {
  df <- readRDS(test_path("data", "animals_count.rds"))
  df_dupes <- readRDS(test_path("data", "animals_undp.rds"))$dupesets
  expect_equal(count_conflicts(df_dupes, dupe_id = "anml_id", silent = T), df)
})

test_that("output as expected", {
  df <- readRDS(test_path("data", "animals_isolate.rds"))
  df_dupes <- readRDS(test_path("data", "animals_undp.rds"))$dupesets
  expect_equal(isolate_conflicts(df_dupes, var = c("number", "row_id"), dupe_id = "anml_id", silent = T), df)
})

test_that("output as expected", {
  df <- readRDS(test_path("data", "animals_flat.rds"))
  df_dupes <- readRDS(test_path("data", "animals_undp.rds"))$dupesets
  expect_equal(flatten_conflicts(df_dupes, dupe_id = "anml_id", silent = T), df)
})
