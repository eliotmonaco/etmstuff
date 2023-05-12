prox_check <- function(x, y, pct=5, n=2, side="both") {

  if (!is.numeric(x) | !is.numeric(y) | !is.numeric(pct) | !is.numeric(n)) {
    stop("`x`, `y`, `pct`, & `n` must be numeric", call. = F)
  }

  pct_above <- x >= y & x <= y + y * (pct/100)
  pct_below <- x <= y & x >= y - y * (pct/100)
  n_above <- x >= y & x <= y + n
  n_below <- x <= y & x >= y - n

  if (side == "both") {
    pct_above | pct_below | n_above | n_below
  } else if (side == "above") {
    pct_above | n_above
  } else if (side == "below") {
    pct_below | n_below
  } else {
    stop("`side` must be one of c(\"both\", \"above\", \"below\")", call. = F)
  }

}
