#' Check date sequences in a dataframe
#'
#' This function checks the sequence of any dates added to the `dates` argument. Any number of date variables can be checked. Equivalent dates are evaluated as being in sequence.
#'
#' @param df A dataframe.
#' @param dates A character vector containing the names of date variables in `df` listed in the expected sequence, i.e., early to late.
#' @param row_id A unique row identifier variable in `df` (optional).
#'
#' @return If any dates are out of sequence, a dataframe containing the affected rows is returned. Only the `row_id` and `dates` variables are returned from `df`.
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' rows <- 100
#'
#' df <- data.frame(
#'   id = 1:rows,
#'   d1 = sample(
#'     seq.Date(
#'       from = as.Date("2020-01-01"),
#'       to = as.Date("2022-12-31"),
#'       by = "day"
#'     ),
#'     size = rows
#'   ),
#'   d2 = sample(
#'     seq.Date(
#'       from = as.Date("2020-01-01"),
#'       to = as.Date("2022-12-31"),
#'       by = "day"
#'     ),
#'     size = rows
#'   )
#' )
#'
#' df_check <- check_date_seq(df, dates = c("d1", "d2"), row_id = "id")
#'
check_date_seq <- function(df, dates, row_id = NULL) {
  var_check(df, var = c(dates, row_id))

  df <- df %>%
    dplyr::select(tidyselect::all_of(c(dates, row_id)))

  f <- function(r) {
    v <- stats::na.omit(as.numeric(as.Date(r)))
    all(v == cummax(v))
  }

  df$dates_in_seq <- apply(
    df %>%
      dplyr::select(tidyselect::all_of(dates)),
    MARGIN = 1,
    FUN = f
  )

  df <- df[which(!Vectorize(isTRUE)(df$dates_in_seq)), ]

  if (nrow(df) == 0) {
    return(message("Dates are in the expected sequence in all rows"))
  }

  df
}
