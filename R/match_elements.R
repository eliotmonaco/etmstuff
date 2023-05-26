#' Check for matching elements in two vectors
#'
#' @param v1 A vector of any type.
#' @param v2 A vector of any type.
#'
#' @return Returns a message stating if elements in the vectors match, and if not, returns a list of which elements do not.
#' @export
#'
#' @examples
#' x <- sample(state.name, size = 40)
#' y <- sample(state.name, size = 40)
#' match_elements(x, y)
#'
#' x <- sample(1:30, size = 20)
#' y <- sample(1:30, size = 20)
#' match_elements(x, y)
match_elements <- function(v1, v2) {
  v1 <- unique(v1)
  v2 <- unique(v2)

  v1_in_v2 <- all(v1 %in% v2)
  v2_in_v1 <- all(v2 %in% v1)

  if (!v1_in_v2) {
    v1_missing <- sort(v1[which(!v1 %in% v2)])
    message("`v1` elements not in `v2`:")
    print(v1_missing)
  } else {
    message("All `v1` elements are in `v2`")
  }

  if (!v2_in_v1) {
    v2_missing <- sort(v2[which(!v2 %in% v1)])
    message("`v2` elements not in `v1`:")
    print(v2_missing)
  } else {
    message("All `v2` elements are in `v1`")
  }
}
