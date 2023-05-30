#' Create a sequence that avoids some numbers
#'
#' `skipper_seq` generates a numeric sequence skipping over any integers provided in `skip`.
#'
#' @param start An integer: the first number in the sequence.
#' @param length An integer: the length of the sequence.
#' @param skip A vector of integers that should not be included in the sequence.
#'
#' @return A numeric vector of length `length`.
#' @export
#'
#' @examples
#' s <- skipper_seq(
#'   start = 1000,
#'   length = 50,
#'   skip = c(1000, 1004:1008, 1045:1055)
#' )
#'
skipper_seq <- function(start, length, skip) {
  s1 <- seq(from = start, length.out = length)
  s2 <- s1[which(!s1 %in% skip)]
  diff <- length(s1) - length(s2)

  while (diff > 0) {
    start2 <- max(s2) + 1

    while (start2 %in% skip) {
      start2 <- start2 + 1
    }

    s2 <- c(s2, seq(from = start2, length.out = diff))
    s2 <- s2[which(!s2 %in% skip)]
    diff <- length(s1) - length(s2)
  }

  s2
}
