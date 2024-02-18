#' Check, clean, and parse test results
#'
#' * `check_results()` finds unexpected test result values and returns a logical vector where `TRUE` indicates a valid result.
#' * `clean_results()` cleans common errors and converts "Not/None detected" to `< 1.0`.
#' * `parse_results()` separates the greater- or less-than sign (`>` or `<`) from the number in the result.
#'
#' @param results A vector of test results.
#'
#' @return A vector the same length as `results`.
#' * `check_results()` and `clean_results()` return a vector the same length as `results`.
#' * `parse_results()` returns a list containing two vectors, `sign` and `number`, the same length as `results`.
#'
# @examples
#'
#' @name test_results
NULL

#' @export
#' @rdname test_results
#' @family lab result processing functions
check_results <- function(results) {
  # Valid pattern (with spaces removed)
  pattern <- "^[<>]?(\\d+|\\d*\\.\\d+)$"

  # Remove spaces and check result
  stringr::str_detect(stringr::str_remove_all(results, "\\s"), pattern)
  }

#' @export
#' @rdname test_results
#' @family lab result processing functions
clean_results <- function(results) {
  # Replace "not/none detected" with "< 1.0"
  pattern <- "(?i)(not|none) detected"
  results <- stringr::str_replace(results, pattern, "< 1.0")

  # Remove units ("mcg/dL" and "ug/dL")
  pattern <- "(?i)(mc|u)g/dl"
  results <- stringr::str_remove_all(results, pattern)

  # If ">" precedes a number below 3.5, replace with "<"
  results <- dplyr::if_else(
    condition = stringr::str_detect(results, "^>") &
      suppressWarnings(as.numeric(stringr::str_remove(results, ">"))) < 3.5,
    true = stringr::str_replace(results, ">", "<"),
    false = results,
    missing = results
  )

  # Remove "=" (as first character), any spaces, punctuation or "`" (as last character)
  pattern <- paste("^=", "\\s", "([:punct:]|`)$", sep = "|")
  results <- stringr::str_remove_all(results, pattern)

  results
}

#' @export
#' @rdname test_results
#' @family lab result processing functions
parse_results <- function(results) {
  # Extract "<" or ">"
  sign <- stringr::str_extract(results, "^[<>]")

  # Extract number
  number <- suppressWarnings(as.numeric(stringr::str_remove(results, "^[<>]")))

  if (any(is.na(number))) warning("`lab_result_number` contains NAs")

  list(
    sign = sign,
    number = number
  )
}
