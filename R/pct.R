#' Calculate percentage
#'
#' This function calculates a percentage using the formula \eqn{n * 100 / total}.
#'
#' @param n,total Numeric vectors, or objects that can be coerced to numeric vectors. If the lengths are unequal, the shorter vector must have length of 1.
#' @param digits An integer to determine the number of decimal places in the output.
#'
#' @return A numeric vector the same length as the longer of `n` or `total.`
#' @export
#'
#' @examples
#' x <- sample(200:300, size = 10, replace = TRUE)
#'
#' pct(x, 300, digits = 2)
#'
pct <- function(n, total, digits = 1) {
  if (!is.numeric(digits) || length(digits) != 1 || digits != round(digits)) {
    stop("`digits` must be an integer")
  }

  if (length(n) != length(total) & length(n) != 1 & length(total) != 1) {
    stop("When the length of `n` & `total` is unequal, the shorter vector must have length of 1")
  }

  n <- as.numeric(n)
  total <- as.numeric(total)

  round(n * 100 / total, digits = digits)
}
