#' Compare via both fuzzy and exact methods
#'
#' @description
#' This function compares variables in two dataframes, allowing some variables to be matched exactly and other variables to be compared using string similarity scoring. This was designed for comparing addresses since certain values, e.g., city, state, and zip code, are more amenable to exact matching, whereas other values, e.g., street and unit, are subject to variation and may require a flexible/inexact method of comparison.
#'
#' The variables undergoing comparison must have the same names in both dataframes (see example). Variables in `exact_var` are matched first if provided. Only rows whose values in `exact_var` match exactly will be returned.
#'
#' @param df1,df2 Dataframes to be compared.
#' @param row_id A unique row identifier variable in `df1` and `df2`.
#' @param fuzzy_var Variables in `df1` and `df2` selected for fuzzy comparison.
#' @param exact_var Variables in `df1` and `df2` selected for exact matching (optional).
#' @param ignore_case Logical: ignore case when comparing fuzzy and exact variables if `TRUE`.
#'
#' @return A dataframe of potential matches joined by the variables in `exact_var`. It contains:
#' * `row_id`s from `df1` and `df2`
#' * `sim_score`: the string similarity score from comparing the values in `fuzzy_var` (see [stringdist::stringsim()])
#' * `string_1` & `string_2`: the merged values from `fuzzy_var` in `df1` and `df2`
#' * all `exact_var` variables (one set only because these values must match in `df1` and `df2` to be returned)
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' df1 <- sim_address(n = 10)
#' df1$id <- 1:nrow(df1)
#'
#' df2 <- sim_address(n = 5000)
#' df2$id <- 1:nrow(df2)
#'
#' df_match <- fuzzy_compare(
#'   df1, df2, row_id = "id",
#'   fuzzy_var = c("street", "unit"),
#'   exact_var = c("city", "state", "zip")
#' )
#'
fuzzy_compare <- function(df1, df2, row_id, fuzzy_var, exact_var = NULL, ignore_case = TRUE) {
  all_vars <- c(row_id, fuzzy_var, exact_var)

  var_check(df1, var = all_vars)
  var_check(df2, var = all_vars)

  # Convert variables being compared to strings, and optionally to upper case
  if (ignore_case) {
    f <- function(x) toupper(as.character(x))
  } else {
    f <- function(x) as.character(x)
  }

  df1 <- df1 %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(c(fuzzy_var, exact_var)), f))

  df2 <- df2 %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(c(fuzzy_var, exact_var)), f))

  # Variable names after inner_join()
  fuzzy_vars1 <- paste0(fuzzy_var, "_1")
  fuzzy_vars2 <- paste0(fuzzy_var, "_2")
  id1 <- paste0(row_id, "_1")
  id2 <- paste0(row_id, "_2")

  # List to hold dataframes created in loop
  match_list <- list()

  for (i in 1:nrow(df1)) {
    # Match rows via inner join using `exact_var`
    match_list[[i]] <- df1[i, all_vars] %>%
      dplyr::inner_join(
        df2[, all_vars],
        by = exact_var,
        suffix = c("_1", "_2"),
        relationship = "one-to-many"
      )

    # Merge fuzzy values into single strings
    str1 <- stringify(match_list[[i]], fuzzy_vars1)
    str2 <- stringify(match_list[[i]], fuzzy_vars2)

    # Score similarity of merged fuzzy values
    match_list[[i]]$sim_score <- stringdist::stringsim(str1, str2)

    match_list[[i]]$string_1 <- str1
    match_list[[i]]$string_2 <- str2
  }

  final_vars <- c(id1, id2, "sim_score", "string_1", "string_2", exact_var)

  purrr::list_rbind(match_list) %>%
    dplyr::select(tidyselect::all_of(final_vars)) %>%
    dplyr::arrange(dplyr::desc(.data$sim_score))
}

stringify <- function(df, col) {
  df %>%
    tidyr::unite(
      tidyselect::all_of(col),
      col = "c", sep = " ", na.rm = TRUE
    ) %>%
    dplyr::pull("c")
}
