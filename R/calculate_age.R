#' Calculate age
#'
#' Calculate age from two date vectors. The formula used is `(d2 - d1) / 365.25`.
#'
#' @param d1 A vector containing the birth or start date for the age calculation.
#' @param d2 A vector containing the end date for the age calculation.
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
#' calculate_age(d1 = start, d2 = end)
#'
calculate_age <- function(d1, d2) {
  # Check if vectors are formatted as dates
  if (!lubridate::is.Date(d1) | !lubridate::is.Date(d2)) {
    stop("`d1` and `d2` must be formatted as dates")
  }

  # Check if vectors are the same length
  if (length(d1) != length(d2)) {
    stop("`d1` and `d2` must be the same length")
  }

  # Check for NAs
  if (any(is.na(d1))) message("NAs found in `d1`")
  if (any(is.na(d2))) message("NAs found in `d2`")

  # Calculate age
  as.numeric((d2 - d1) / 365.25)
}
