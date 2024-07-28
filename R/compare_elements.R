#' Compare values, vectors, and dataframes
#'
#' @param x,y Values to be compared.
#' @param v1,v2 Vectors of the same length to be compared.
#' @param df1,df2 Dataframes of the same dimensions to be compared.
#' @param ignore A vector of values to ignore in the comparison.
#' @param fn A function that determines the output of `compare_fn()`.
#' @param numeric The type of output when comparing unequal numeric values.
#'
#' * `"values"` returns both input values in a string separated by "|"
#' * `"diff"` returns the difference between the input values
#'
#' @return An object with the same dimensions as the input objects. All functions return `NA` when one of the input values is included in the `ignore` argument. Otherwise, `compare_val()` returns a string containing both input values separated by "|". `compare_diff()` returns the difference between numeric inputs. `compare_tf()` returns `TRUE` if the inputs are identical and `FALSE` if the inputs are not. `compare_fn()` returns a value depending on the function provided to the argument `fn`.
#'
#' @examples
#' df1 <- data.frame(
#'   col1 = letters[1:9],
#'   col2 = TRUE,
#'   col3 = as.numeric(1:9)
#' )
#'
#' df2 <- data.frame(
#'   col_1 = c(letters[1:3], LETTERS[4:6], letters[7:9]),
#'   col_2 = NA,
#'   col_3 = as.numeric(c(1:2, 30, 4:6, 70, 80, 9))
#' )
#'
#' comp1 <- compare_dfs(df1, df2)
#' comp2 <- compare_dfs(df1, df2, ignore = NA)
#' comp3 <- compare_dfs(df1, df2, ignore = NA, numeric = "diff")
#'
#' fn_new <- function(x, y) {
#'   paste("x is", x, "but y is", y)
#' }
#'
#' compare_fn(1, 2, fn_new)
#'
#' @name compare_elements
NULL

#' @export
#' @rdname compare_elements
compare_val <- function(x, y, ignore = NULL) {
  if (any(c(x, y) %in% ignore)) {
    NA
  } else {
    if (identical(x, y)) {
      NA
    } else {
      paste(x, y, sep = " | ")
    }
  }
}

#' @export
#' @rdname compare_elements
compare_diff <- function(x, y, ignore = NULL) {
  if (any(c(x, y) %in% ignore)) {
    NA
  } else {
    if (identical(x, y)) {
      NA
    } else {
      as.numeric(x) - as.numeric(y)
    }
  }
}

#' @export
#' @rdname compare_elements
compare_tf <- function(x, y, ignore = NULL) {
  if (any(c(x, y) %in% ignore)) {
    NA
  } else {
    identical(x, y)
  }
}

#' @export
#' @rdname compare_elements
compare_fn <- function(x, y, fn, ignore = NULL) {
  if (any(c(x, y) %in% ignore)) {
    NA
  } else {
    if (identical(x, y)) {
      NA
    } else {
      fn(x, y)
    }
  }
}

#' @export
#' @rdname compare_elements
compare_vectors <- function(v1, v2, ignore = NULL, numeric = "values") {
  if (length(v1) != length(v2)) {
    stop("`v1` and `v2` must have the same length")
  }

  if (numeric == "values") {
    unlist(purrr::map2(v1, v2, compare_val, ignore = ignore))
  } else if (numeric == "diff" && is.numeric(v1) && is.numeric(v2)) {
    unlist(purrr::map2(v1, v2, compare_diff, ignore = ignore))
  } else if (numeric == "diff") {
    unlist(purrr::map2(v1, v2, compare_val, ignore = ignore))
  } else {
    stop("`numeric` must be one of c(\"values\", \"diff\")")
  }
}

#' @export
#' @rdname compare_elements
compare_dfs <- function(df1, df2, ignore = NULL, numeric = "values") {
  if (ncol(df1) != ncol(df2) || nrow(df1) != nrow(df2)) {
    stop("`df1` and `df2` must have the same dimensions")
  }

  results <- list()

  # Column-wise comparisons
  for (i in 1:ncol(df1)) {
    results[[i]] <- compare_vectors(df1[[i]], df2[[i]], ignore, numeric)
  }

  # If column names aren't identical, use both
  if (identical(colnames(df1), colnames(df2))) {
    cols <- colnames(df1)
  } else {
    cols <- paste(colnames(df1), colnames(df2), sep = " | ")
  }

  # Get the number of differences between dataframes
  n_vals <- sum(unlist(purrr::map(results, length)))
  n_identical <- sum(unlist(purrr::map(results, is.na)))
  n_diffs <- n_vals - n_identical
  message(paste("Differences in dataframes:", n_diffs))

  names(results) <- cols
  tibble::as_tibble(results)
}
