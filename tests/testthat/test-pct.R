test_that("numeric vectors", {
  expect_equal(
    pct(
      c(0, 1, 2, 3, 4, 5, 6, 7, 8),
      c(8, 7, 6, 5, 4, 3, 2, 1, 0),
      digits = 2
    ),
    c(0.00, 14.29, 33.33, 60.00, 100.00, 166.67, 300.00, 700.00, Inf)
  )
})

test_that("lists containing multiple classes", {
  expect_equal(
    suppressWarnings(pct(
      list(0, 1, "2", 3, 4, "5", 6, "A", TRUE),
      list(FALSE, "B", "6", 5, "4", 3, 2, 1, 0)
    )),
    c(NaN, NA, 33.3, 60.0, 100.0, 166.7, 300.0, NA, Inf)
  )
})

test_that("vectors of unequal length", {
  expect_equal(pct(c(1, 2, 3, 4), 12), c(8.3, 16.7, 25.0, 33.3))
})

test_that("vectors of unequal length (error)", {
  expect_error(
    pct(c(1, 2, 3, 4), c(1, 2)),
    "When the length of `n` & `total` is unequal, the shorter vector must have length of 1"
  )
})

test_that("digits is character", {
  expect_error(
    pct(1, 2, digits = "1"),
    "`digits` must be an integer"
  )
})

test_that("digits is double", {
  expect_error(
    pct(1, 2, digits = 1.1),
    "`digits` must be an integer"
  )
})

test_that("digits has length > 1", {
  expect_error(
    pct(1, 2, digits = 1:2),
    "`digits` must be an integer"
  )
})
