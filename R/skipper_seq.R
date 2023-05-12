# Creates a sequence that skips any integers provided in `skip`.

skipper_seq <- function(start, length, skip) {

  s1 <- seq(from = start, length.out = length)
  s2 <- s1[which(!s1 %in% skip)]
  diff <- length(s1) - length (s2)

  while (diff > 0) {

    start2 <- max(s2) + 1

    while (start2 %in% skip) {
      start2 <- start2 + 1
    }

    s2 <- c(s2, seq(from = start2, length.out = diff))
    s2 <- s2[which(!s2 %in% skip)]
    diff <- length(s1) - length (s2)

  }

  s2

}



