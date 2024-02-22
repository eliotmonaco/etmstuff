test_that("output as expected", {
  df <- readRDS(test_path("data", "animals.rds"))
  undp <- readRDS(test_path("data", "animals_undp.rds"))
  expect_equal(undupe(df, var = c("name", "date"), prefix = "anml"), undp)
})

test_that("output (df_full) similar to input", {
  df <- readRDS(test_path("data", "animals.rds"))
  df_full <- undupe(df, var = c("name", "date"), prefix = "anml")$full
  expect_equal(df_full[!colnames(df_full) %in% c("anml_id", "anml_order")], df)
})

test_that("prefix arg error", {
  df <- readRDS(test_path("data", "animals.rds"))
  expect_error(
    undupe(df, var = c("name", "date"), prefix = "row"),
    "Provide a different `prefix`"
  )
})

test_that("prefix arg error", {
  df <- tibble::tibble(x = 1:10, y = letters[1:10])
  expect_message(
    undupe(df, var = c("x", "y")),
    "No duplicates were found"
  )
})
