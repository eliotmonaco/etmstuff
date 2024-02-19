test_that("function output as expected", {
  df <- readRDS(test_path("data", "animals.rds"))
  expect_equal(
    id_distinct_rows(
      df[1:3], id_name = "row_id", prefix = "X",
      var = c("animals", "dates")
    ),
    df
  )
})

test_that("id_name already exists error", {
  df <- readRDS(test_path("data", "animals.rds"))
  id_name <- "animals"
  m <- paste0("`", id_name, "` is already a column in `df`. Choose a different `id_name`.")
  expect_error(id_distinct_rows(df, id_name = "animals"), m)
})
