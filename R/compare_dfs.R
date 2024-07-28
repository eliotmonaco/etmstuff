#' Compare dataframes with some flexibility
#'
#' This function allows for specified values to be ignored and for flexibility in output values when comparing two dataframes.
#'
#' When the compared values from the input dataframes are equal, or when one is included in the `ignore` argument, `NA` is returned in the appropriate position in the output dataframe. When the compared values are unequal, the value returned is determined by the `output` argument.
#'
#' @param df1,df2 Dataframes to compare (must have the same dimensions).
#' @param ignore A value or a list of values to ignore in the comparison.
#' @param output A string or function that determines the output when unequal values are compared.
#'
#' * `"values"` returns both input values in a string separated by "|".
#' * `"diff"` returns the difference between the input values when both are numeric.
#' * A function whose arguments are `x` and `y` can be provided.
#'
#' @return A dataframe with the same dimensions as the inputs.
#' @export
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
#' comp3 <- compare_dfs(df1, df2, ignore = NA, output = "diff")
#' comp4 <- compare_dfs(df1, df2, ignore = NA, output = function(x, y) paste("x is", x, "but y is", y))
#'
compare_dfs <- function(df1, df2, ignore = NULL, output = "values") {
  if (ncol(df1) != ncol(df2) || nrow(df1) != nrow(df2)) {
    stop("`df1` and `df2` must have the same dimensions")
  }

  results <- list()

  # Column-wise comparisons
  if (rlang::is_function(output)) {
    for (i in 1:ncol(df1)) {
      results[[i]] <- unlist(purrr::map2(df1[[i]], df2[[i]], compare_fn, fn = output, ignore = ignore))
    }
  } else if (output == "values") {
    for (i in 1:ncol(df1)) {
      results[[i]] <- unlist(purrr::map2(df1[[i]], df2[[i]], compare_val, ignore = ignore))
    }
  } else if (output == "diff") {
    for (i in 1:ncol(df1)) {
      if (is.numeric(df1[[i]]) && is.numeric(df2[[i]])) {
        results[[i]] <- unlist(purrr::map2(df1[[i]], df2[[i]], compare_diff, ignore = ignore))
      } else {
        results[[i]] <- unlist(purrr::map2(df1[[i]], df2[[i]], compare_val, ignore = ignore))
      }
    }
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

compare_tf <- function(x, y, ignore = NULL) {
  if (any(c(x, y) %in% ignore)) {
    NA
  } else {
    identical(x, y)
  }
}

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
