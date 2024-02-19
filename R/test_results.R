#' Check, clean, and parse test results
#'
#' * `check_results()` finds results that don't match the expected pattern of a greater- or less-than sign and a number.
#' * `clean_results()` cleans common errors and converts "Not/None detected" to `< 1.0`.
#' * `parse_results()` separates the greater- or less-than sign from the number in the cleaned results.
#' * `flag_results()` finds parsed results that require review.
#'
#' @param results A vector of test results.
#' @param sign The `sign` vector returned by `parse_results()`
#' @param number The `number` vector returned by `parse_results()`
#'
#' @return
#' * `check_results()` and `flag_results()` return a logical vector the same length as the argument(s).
#' * `clean_results()` returns a character vector the same length as the argument.
#' * `parse_results()` returns a list containing `sign` (a character vector) and `number` (a numeric vector), each the same length as the argument.
#'
# @examples
#'
#' @name test_results
NULL

#' @export
#' @rdname test_results
#' @family test result processing
check_results <- function(results) {
  # Valid pattern (with spaces removed)
  pattern <- "^[<>]?(\\d+|\\d*\\.\\d+)$"

  # Remove spaces and check result
  stringr::str_detect(stringr::str_remove_all(results, "\\s"), pattern)
  }

#' @export
#' @rdname test_results
#' @family test result processing
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
#' @family test result processing
parse_results <- function(results) {
  # Extract "<" or ">"
  sign <- stringr::str_extract(results, "^[<>]")

  # Extract number
  number <- suppressWarnings(as.numeric(stringr::str_remove(results, "^[<>]")))

  list(
    sign = sign,
    number = number
  )
}

#' @export
#' @rdname test_results
#' @family test result processing
flag_results <- function(sign, number) {
  if (length(sign) != length(number)) {
    stop("`sign` and `number` must be vectors of the same length")
  }

  dplyr::case_when(
    number == 0 ~ TRUE,
    number > 100 ~ TRUE,
    is.na(number) ~ TRUE,
    sign == ">" ~ TRUE,
    sign == "<" &
      (number != 1 & number != 2 &
         number != 3 & number != 3.3) ~ TRUE,
    TRUE ~ FALSE
  )
}
