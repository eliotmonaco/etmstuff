#' Check date sequences
#'
#' This function checks whether or not a sequence of dates is in order from earliest to latest. Any number of date vectors can be checked. The order in which arguments are evaluated is `date1`, `date2`, and then any additional arguments from left to right. Equivalent dates are evaluated as being in sequence. Sequences with `NA`s are evaluated with `NA`s removed.
#'
#' @param date1,date2 Vectors containing dates. Elements at the same index will be compared, i.e., `date1[1]` and `date2[1]`.
#' @param ... Additional date vectors to compare.
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
check_date_seq <- function(date1, date2, ...) {
  df <- cbind(date1, date2, ...)

  f <- function(dates) {
    # Convert a vector of dates to the underlying numeric values, removing NAs
    seq <- stats::na.omit(as.numeric(as.Date(dates)))
    # Compare the current order of `seq` to the sorted order (small to large)
    all(seq == sort(seq))
  }

  apply(df, 1, f)
}
