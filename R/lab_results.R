#' Check, clean, and parse lab results
#'
#' * `check_lab_results()` finds unexpected values in the lab results and returns a dataframe with only those records.
#' * `clean_lab_results()` cleans common errors in the results and converts "Not/None detected" to `0`. The cleaned results appear in a new variable, `lab_results_cleaned`.
#' * `parse_lab_results()` creates two new variables: `lab_results_symbol`, containing the comparison operator (`<` or `>`), and `lab_results_number`, containing the numeric result.
#'
#' @param df A dataframe.
#' @param var The variable name from `df` with lab results to check, clean, or parse.
#'
#' @return A dataframe.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
# @examples
#'
#' @name lab_results
NULL

#' @export
#' @rdname lab_results
#' @family lab result processing functions
check_lab_results <- function(df, var) {
  var_check(df, var = c("lab_result_value", var))

  # Valid pattern
  p <- paste0("^(<|>)?", "(\\d+|\\d*\\.\\d+)", "$")

  # Return rows with unexpected pattern in result (remove spaces before checking)
  df %>%
    dplyr::filter(!stringr::str_detect(stringr::str_remove_all(.data[[var]], "\\s"), p)) %>%
    dplyr::relocate(tidyselect::starts_with("lab_result")) %>%
    dplyr::arrange(.data[[var]])
}

#' @export
#' @rdname lab_results
#' @family lab result processing functions
clean_lab_results <- function(df, var) {
  var_check(df, var = c("lab_result_value", var))

  # Replace or remove text in results
  f <- function(var) {
    dplyr::case_when(
      # Replace "not/none detected" with "< 1.0"
      stringr::str_detect(var, stringr::regex("(not|none) detected", ignore_case = TRUE)) ~ "< 1.0",
      # Remove units ("mcg/dL" and "ug/dL")
      stringr::str_detect(var, stringr::regex("(mc|u)g/dl", ignore_case = TRUE)) ~
        stringr::str_remove_all(var, stringr::regex("(mc|u)g/dl", ignore_case = TRUE)),
      # If ">" operator precedes a number below 3.5, replace it with "<"
      stringr::str_detect(var, "^>") & as.numeric(stringr::str_remove(var, ">")) < 3.5 ~
        stringr::str_replace(var, ">", "<"),
      T ~ var
    )
  }

  # Characters to remove: "=" (first character), spaces, punctuation or "`" (last character)
  p <- paste("^=", "\\s", "([:punct:]|`)$", sep = "|")

  df %>%
    dplyr::mutate(lab_result_clean = f(.data[[var]])) %>%
    dplyr::mutate(lab_result_clean = stringr::str_remove_all(.data$lab_result_clean, p)) %>%
    dplyr::relocate("lab_result_clean", .after = tidyselect::all_of(var))
}

#' @export
#' @rdname lab_results
#' @family lab result processing functions
parse_lab_results <- function(df, var) {
  var_check(df, var = c("lab_result_value", var))

  # Extract comparison symbol
  df$lab_result_symbol <- stringr::str_extract(df[[var]], "^(<|>)")

  # Extract number
  df$lab_result_number <- as.numeric(stringr::str_remove(df[[var]], "^(<|>)"))

  if (any(is.na(df$lab_result_number))) message("`lab_result_number` contains NAs")

  df %>%
    dplyr::relocate("lab_result_symbol", "lab_result_number", .after = tidyselect::all_of(var))
}
