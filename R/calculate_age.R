#' Calculate age
#'
#' Calculate age from two date vectors. The formula used is `(date2 - date1) / 365.25`.
#'
#' @param date1 A vector containing the birth or start date for the age calculation.
#' @param date2 A vector containing the end date for the age calculation.
#'
#' @return A numeric vector.
#' @export
#'
#' @examples
#' start <- sample(
#'   seq.Date(
#'     from = as.Date("1970-01-01"),
#'     to = as.Date("1980-01-01"),
#'     by = "day"
#'   ),
#'   size = 10
#' )
#'
#' end <- sample(
#'   seq.Date(
#'     from = as.Date("1990-01-01"),
#'     to = as.Date("2020-01-01"),
#'     by = "day"
#'   ),
#'   size = 10
#' )
#'
#' calculate_age(date1 = start, date2 = end)
#'
calculate_age <- function(date1, date2) {
  # Check if vectors are formatted as dates
  if (!lubridate::is.Date(date1) | !lubridate::is.Date(date2)) {
    stop("`date1` and `date2` must be formatted as dates")
  }

  # Check if vectors are the same length
  if (length(date1) != length(date2)) {
    stop("`date1` and `date2` must be the same length")
  }

  # Check for NAs
  if (any(is.na(date1))) message("NAs found in `date1`")
  if (any(is.na(date2))) message("NAs found in `date2`")

  # Calculate age
  as.numeric((date2 - date1) / 365.25)
}
