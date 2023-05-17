#' Check, clean, and parse lab results
#'
#' * `check_lab_results()`
#' * `clean_lab_results()`
#' * `parse_lab_results()`
#'
#' @param df A dataframe.
#' @param var A variable name from `df` containing lab results.
#'
#' @return A dataframe.
#'
#' @importFrom tidyr unite
#'
# @examples
#'
#' @name lab_results
NULL

#' @export
#' @rdname lab_results
check_lab_results <- function(df, var) {

  var_check(df, var = c("lab_result_value", var))

  # Return rows with unexpected formatting in result
  df %>%
    filter(!str_detect(.data[[var]], "^(<|>)?\\s*\\d*\\.?\\d*\\s*$") |
             !str_detect(.data[[var]], "\\d")) %>%
    relocate(starts_with("lab_result")) %>%
    arrange(.data[[var]])

}

#' @export
#' @rdname lab_results
clean_lab_results <- function(df, var) {

  var_check(df, var = var)

  # Replace or remove text in results
  f <- function(var) {
    case_when(str_detect(var, regex("(not|none) detected", ignore_case = T)) ~ "0",
              str_detect(var, regex("(mc|u)g/dl", ignore_case = T)) ~
                str_remove_all(var, regex("(mc|u)g/dl", ignore_case = T)),
              str_detect(var, "^>") & as.numeric(str_remove(var, ">")) < 3.5 ~
                str_replace(var, ">", "<"),
              T ~ var)
  }

  df %>%
    mutate(lab_result_clean = f(.data[[var]])) %>%
    mutate(lab_result_clean = str_remove_all(lab_result_clean, "^=|\\s|([:punct:]|`)$")) %>%
    relocate(lab_result_clean, .after = all_of(var))

}

#' @export
#' @rdname lab_results
parse_lab_results <- function(df, var) {

  var_check(df, var = c("recno", var))

  # Extract comparison symbol
  extracted <- str_extract_all(df[[var]], "<|>", simplify = T)
  df <- cbind(df, tidyr::unite(as.data.frame(extracted), "lab_result_symbol", sep = " "))
  df$lab_result_symbol[df$lab_result_symbol == ""] <- NA

  # Extract number
  df$lab_result_number <- as.numeric(str_remove_all(df[[var]], "[^\\d\\.]"))

  # Check lab_result_number for any non-numeric value
  df_prob <- df %>%
    filter(!str_detect(lab_result_number, "^\\d+$|^\\d*\\.\\d+$") |
             is.na(lab_result_number)) %>%
    select(recno)

  if (nrow(df_prob) == 0) {
    m <- "All lab results parsed"
  } else {
    m <- paste("Problems with the following record(s):", paste(df_prob$recno, collapse = ", "))
  }

  message(m)

  df %>%
    relocate(lab_result_symbol, lab_result_number, .after = all_of(var))

}
