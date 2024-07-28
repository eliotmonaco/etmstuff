test_that("compare_val works", {
  expect_equal(compare_val(1, 1), NA)
  expect_equal(compare_val(1, 2), "1 | 2")
  expect_equal(compare_val(1, 2, ignore = 2), NA)
  expect_equal(compare_val("a", "b", ignore = list("b", 2)), NA)
})

test_that("compare_diff works", {
  expect_equal(compare_diff(1, 2), -1)
  expect_equal(compare_val(1, 2, ignore = list("b", 2)), NA)
})

test_that("compare_tf works", {
  expect_equal(compare_tf(1, 1), TRUE)
  expect_equal(compare_tf(1, 2), FALSE)
  expect_equal(compare_tf(1, 2, ignore = 2), NA)
  expect_equal(compare_val("a", "b", ignore = list("b", 2)), NA)
})

test_that("compare_fn works", {
  expect_equal(compare_fn(1, 2, fn = function(x, y) paste("x is", x, "but y is", y)), "x is 1 but y is 2")
})

test_that("compare_dfs works", {
  df1 <- data.frame(col1 = c("a", "b"), col2 = c(1, 2))
  df2 <- data.frame(col_1 = c("a", "c"), col_2 = c(2, 2))
  expect_equal(
    compare_dfs(df1, df2),
    tibble::tibble(
      "col1 | col_1" = c(NA, "b | c"),
      "col2 | col_2" = c("1 | 2", NA)
    )
  )
  expect_equal(
    compare_dfs(df1, df2, output = "diff"),
    tibble::tibble(
      "col1 | col_1" = c(NA, "b | c"),
      "col2 | col_2" = c(-1, NA)
    )
  )
  expect_equal(
    compare_dfs(df1, df2, output = function(x, y) paste("x is", x, "but y is", y)),
    tibble::tibble(
      "col1 | col_1" = c(NA, "x is b but y is c"),
      "col2 | col_2" = c("x is 1 but y is 2", NA)
    )
  )
})
