#' Check the proximity of two numbers
#'
#' Returns a logical value indicating whether or not `x` is within a specified range of `y`.
#'
#' The proximity of `x` to `y` is checked for two range types: a percent (`pct`) of `y` and a number (`n`). `prox_check()` returns `TRUE` if `x` is within either range. If only one range type should be evaluated, set the other type to `0`. `side` determines whether the range is evaluated above `y`, below `y`, or both.
#'
#' @param x A number.
#' @param y A number.
#' @param pct A number defining a range to check as a percent of `y`.
#' @param n A number defining an absolute range to check.
#' @param side The side of `y` to check: `"above"`, `"below"`, or `"both"` (default).
#'
#' @return A logical value.
#' @export
#'
#' @examples
#' prox_check(105, 100)
#'
prox_check <- function(x, y, pct = 5, n = 2, side = "both") {
  if (!is.numeric(x) |
    !is.numeric(y) |
    !is.numeric(pct) |
    !is.numeric(n)) {
    stop("`x`, `y`, `pct`, & `n` must be numeric")
  }

  pct_above <- x >= y & x <= y + y * (pct / 100)
  pct_below <- x <= y & x >= y - y * (pct / 100)
  n_above <- x >= y & x <= y + n
  n_below <- x <= y & x >= y - n

  if (side == "both") {
    pct_above | pct_below | n_above | n_below
  } else if (side == "above") {
    pct_above | n_above
  } else if (side == "below") {
    pct_below | n_below
  } else {
    stop('`side` must be one of c("both", "above", "below")')
  }
}
