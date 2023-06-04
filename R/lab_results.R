#' Check, clean, and parse lab results
#'
#' * `check_lab_results()` finds unexpected values in the results, i.e., anything that isn't a number, sometimes preceded by `<` or `>`.
#' * `clean_lab_results()` removes the most common unexpected values from the results and converts a `Not detected` (or similar) result to `0`. The cleaned results appear in a new variable, `lab_results_cleaned`.
#' * `parse_lab_results()` splits the cleaned lab results into two new variables: `lab_results_symbol`, containing either `<`, `>`, or `NA`, and `lab_results_number`, containing the numeric result.
#'
#' @param df A dataframe.
#' @param var A variable name from `df` containing lab results.
#'
#' @return A dataframe.
#'
#' @importFrom magrittr %>%
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

  p <- "^(<|>)?\\s*\\d*\\.?\\d*\\s*$"

  # Return rows with unexpected formatting in result
  df %>%
    dplyr::filter(!stringr::str_detect(.data[[var]], p) |
      !stringr::str_detect(.data[[var]], "\\d")) %>%
    dplyr::relocate(tidyselect::starts_with("lab_result")) %>%
    dplyr::arrange(.data[[var]])
}

#' @export
#' @rdname lab_results
#' @family lab result processing functions
clean_lab_results <- function(df, var) {
  var_check(df, var = var)

  # Replace or remove text in results
  f <- function(var) {
    dplyr::case_when(
      stringr::str_detect(var, stringr::regex("(not|none) detected", ignore_case = TRUE)) ~ "0",
      stringr::str_detect(var, stringr::regex("(mc|u)g/dl", ignore_case = TRUE)) ~
        stringr::str_remove_all(var, stringr::regex("(mc|u)g/dl", ignore_case = TRUE)),
      stringr::str_detect(var, "^>") & as.numeric(stringr::str_remove(var, ">")) < 3.5 ~
        stringr::str_replace(var, ">", "<"),
      T ~ var
    )
  }

  p <- "^=|\\s|([:punct:]|`)$"

  df %>%
    dplyr::mutate(lab_result_clean = f(.data[[var]])) %>%
    dplyr::mutate(lab_result_clean = stringr::str_remove_all(lab_result_clean, p)) %>%
    dplyr::relocate(lab_result_clean, .after = tidyselect::all_of(var))
}

#' @export
#' @rdname lab_results
#' @family lab result processing functions
parse_lab_results <- function(df, var) {
  var_check(df, var = c("recno", var))

  # Extract comparison symbol
  extracted <- stringr::str_extract_all(df[[var]], "<|>", simplify = TRUE)

  # Convert to column and add to `df`
  df <- cbind(df, tidyr::unite(
    as.data.frame(extracted),
    col = "lab_result_symbol",
    sep = " "
  ))

  df$lab_result_symbol[df$lab_result_symbol == ""] <- NA

  # Extract number
  df$lab_result_number <- as.numeric(stringr::str_remove_all(df[[var]], "[^\\d\\.]"))

  p <- "^\\d+$|^\\d*\\.\\d+$"

  # Check lab_result_number for any non-numeric value
  df_prob <- df %>%
    dplyr::filter(!stringr::str_detect(lab_result_number, p) |
      is.na(lab_result_number)) %>%
    dplyr::select(recno)

  if (nrow(df_prob) == 0) {
    m <- "All lab results parsed"
  } else {
    m <- paste(
      "Problems with the following record(s):",
      paste(df_prob$recno, collapse = ", ")
    )
  }

  message(m)

  df %>%
    dplyr::relocate(
      lab_result_symbol, lab_result_number,
      .after = tidyselect::all_of(var)
    )
}
