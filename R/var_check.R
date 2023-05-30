#' Check variable names in a dataframe
#'
#' This is used mainly inside other functions to check for the presence of necessary variables. If any element of `var` is not present in `df`, `var_check()` stops function execution and returns an error listing the missing variables.
#'
#' @param df A dataframe.
#' @param var A character vector.
#'
#' @return Nothing is returned if all elements of `var` are present in `df`.
#' @export
#'
#' @examples
#' x <- c("height", "age")
#' var_check(Loblolly, var = x)
#'
#' # Strings are case-sensitive
#' try(var_check(Loblolly, "seed"))
#'
var_check <- function(df, var) {
  missing <- var[which(!var %in% colnames(df))]

  if (!purrr::is_empty(missing)) {
    m <- paste(
      "Missing dataframe variable(s):",
      paste(missing, collapse = ", ")
    )
    stop(m, call. = FALSE)
  }
}
