#' Check dataframe columns for a pattern
#'
#' Search a dataframe for a pattern formatted as a regular expression. For exact matches, use `^` and `$` at the beginning and ending of the string in `pattern`.
#'
#' @param df A dataframe.
#' @param pattern A pattern to search for, formatted as a regular expression.
#' @param ignore_case Logical: ignore case in pattern search if `TRUE`.
#'
#' @return Returns a message listing which columns contain the pattern.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   x = c("one", "two", "three"),
#'   y = c("eleven", "twelve", "thirteen"),
#'   z = c("twenty-one", "twenty-two", "twenty-three")
#' )
#' pattern_check_df(df, "^one$")
#'
pattern_check_df <- function(df, pattern, ignore_case = FALSE) {
  # Empty vector to hold names of variables in which `pattern` is found
  var <- c()

  for (i in 1:ncol(df)) {
    pattern_in_col <- any(
      stringr::str_detect(
        df[[i]],
        stringr::regex(pattern, ignore_case = ignore_case)
      ),
      na.rm = TRUE
    )
    if (pattern_in_col) {
      var <- c(var, colnames(df)[i])
    }
    # Progress indicator
    message(
      "\r",
      paste("Column", i, "of", ncol(df), "checked"),
      appendLF = FALSE
    )
  }

  # New line after progress indicator is finished
  message()

  # Output
  if (length(var) == 0) {
    message("Pattern not found in dataframe")
  } else {
    message(paste(
      "Pattern found in:",
      paste(var, collapse = ", ")
    ))
  }
}
