#' Check date sequences
#'
#' This function checks whether or not a sequence of dates is in order from earliest to latest. Any number of date vectors can be checked. The function expects dates to progress through time in argument order from left to right. Elements at the same index will be compared, as in `date1[1] < date2[1]`, `date2[1] < date3[1]`, etc. Equivalent dates are evaluated as being in order. `NA`s are omitted during evaluation.
#'
#' @param ... Vectors containing dates.
#'
#' @return A logical vector of the same length as the arguments.
#' @export
#'
#' @examples
#' dates <- seq.Date(
#'   as.Date("2023-01-01", "%Y-%m-%d"),
#'   as.Date("2023-12-31", "%Y-%m-%d"),
#'   by = "day"
#' )
#' dates <- c(dates, rep(NA, times = 10))
#'
#' n_rows <- 500
#'
#' df <- data.frame(
#'   d1 = sample(dates, n_rows, replace = TRUE),
#'   d2 = sample(dates, n_rows, replace = TRUE),
#'   d3 = sample(dates, n_rows, replace = TRUE),
#'   d4 = sample(dates, n_rows, replace = TRUE)
#' )
#'
#' df$ordered <- check_date_seq(df$d1, df$d2, df$d3, df$d4)
#'
check_date_seq <- function(...) {
  df <- cbind(...)

  f <- function(dates) {
    # Convert a vector of dates to the underlying numeric values, removing NAs
    seq <- stats::na.omit(as.numeric(as.Date(dates)))
    # Compare the current order of `seq` to the sorted order (small to large)
    all(seq == sort(seq))
  }

  apply(df, 1, f)
}
