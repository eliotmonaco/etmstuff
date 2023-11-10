#' Calculate a percentage
#'
#' @param n A number. The numerator of the fraction that will become a percentage.
#' @param total A number. The denominator of the fraction that will become a percentage.
#' @param dec An integer. The number of decimal places to round the percentage to.
#'
#' @return A number.
#' @export
#'
#' @examples
#' pct(5, 25)
#'
#' x <- sample(200:300, size = 10, replace = TRUE)
#' pct(x, total = 300, dec = 2)
#'
pct <- function(n, total, dec = 1) {
  if (!is.numeric(n) | !is.numeric(total) | dec != round(dec)) {
    stop("`n` and `total` must be numeric, and `dec` must be an integer", call. = FALSE)
  }

  round(n * 100 / total, digits = dec)
}
