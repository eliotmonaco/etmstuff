#' Search a dataframe for a pattern
#'
#' Search each column of a dataframe for a pattern using regular expressions.
#'
#' @param df A dataframe.
#' @param pattern A search pattern consisting of regular expressions.
#'
#' @return A message indicating which columns, if any, contain the pattern.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   x = c("one", "two", "three"),
#'   y = c("eleven", "twelve", "thirteen"),
#'   z = c("twenty-one", "twenty-two", "twenty-three")
#' )
#'
#' check_pattern_df(df, "^one$")
#'
check_pattern_df <- function(df, pattern) {
  if (!is.data.frame(df)) stop("`df` must be a dataframe")
  if (length(pattern) != 1) stop("`pattern` must have length of 1")

  # Object to hold names of variables in which `pattern` is found
  var <- c()

  # Check each column for pattern
  for (i in 1:ncol(df)) {
    pattern_in_col <- any(
      stringr::str_detect(df[[i]], pattern),
      na.rm = TRUE
    )
    if (pattern_in_col) {
      var <- c(var, colnames(df)[i])
    }
  }

  if (ncol(df) > 1) {
    m <- paste(ncol(df), "columns checked.")
  } else {
    m <- paste(ncol(df), "column checked.")
  }

  # Message output
  if (length(var) == 0) {
    m <- paste(m, "Pattern not found in dataframe.")
    message(m)
  } else {
    m <- paste(m, paste("Pattern found in:", paste(paste0("`", var, "`"), collapse = ", ")))
    message(m)
  }
}
