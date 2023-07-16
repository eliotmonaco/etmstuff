#' Compare rows using a combination of fuzzy and exact methods
#'
#' @description
#' This function compares rows in two dataframes, allowing some values to be matched exactly and other values to be scored based on their similarity. It was designed for comparing addresses, in which certain values such as the city, state, and zip code are more amenable to exact matching and other values such as the street and unit components are subject to a large degree of variation and may require a flexible or inexact method of comparison.
#'
#' The variables being compared must have the same names in both dataframes. If addresses in `df1` are being compared, e.g., `df1$street`, `df1$city`, and `df1$state`, then `df2` must have the same variable names, `df2$street`, `df2$city`, and `df2$state`.
#'
#' @param df1 A dataframe with rows to be compared.
#' @param df2 A dataframe to be compared against.
#' @param fuzzy_var Names of variables in `df1` and `df2` selected for fuzzy comparison.
#' @param exact_var Names of variables in `df1` and `df2` selected for exact matching (optional).
#' @param ignore_case Logical. Ignore case when comparing fuzzy and exact variables if `TRUE`.
#'
#' @return A dataframe of potential matches joined by the variables in `exact_var`, with the new variable `sim_score`.
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' df1 <- sim_address(nrow = 10)
#' df2 <- sim_address(nrow = 5000)
#'
#' df_match <- fuzzy_compare(
#'   df1,
#'   df2,
#'   fuzzy_var = c("street", "unit"),
#'   exact_var = c("city", "state", "zip")
#' )
#'
fuzzy_compare <- function(df1, df2, fuzzy_var, exact_var = NULL, ignore_case = TRUE) {
  var_check(df1, var = c(fuzzy_var, exact_var))
  var_check(df2, var = c(fuzzy_var, exact_var))

  # Convert variables being compared to character, and optionally to upper case
  if (ignore_case) {
    f <- function(x) {
      toupper(as.character(x))
    }
  } else {
    f <- function(x) {
      as.character(x)
    }
  }

  df1 <- df1 %>%
    dplyr::mutate(dplyr::across(c(
      tidyselect::all_of(fuzzy_var),
      tidyselect::all_of(exact_var)
    ), f))

  df2 <- df2 %>%
    dplyr::mutate(dplyr::across(c(
      tidyselect::all_of(fuzzy_var),
      tidyselect::all_of(exact_var)
    ), f))

  # Vector of names in `fuzzy_var` after inner_join(), to be used in unite()
  fzy1 <- paste0(fuzzy_var, "_1")
  fzy2 <- paste0(fuzzy_var, "_2")

  # List to hold dataframes created in loop
  match_list <- list()

  for (i in 1:nrow(df1)) {
    # Inner join to match rows via `exact_var`
    match_list[[i]] <- df1[i, ] %>%
      dplyr::inner_join(
        df2,
        by = exact_var,
        suffix = c("_1", "_2"),
        relationship = "one-to-many"
      ) %>%
      dplyr::relocate(
        tidyselect::all_of(fzy1),
        tidyselect::all_of(fzy2),
        tidyselect::all_of(exact_var)
      )

    # Merge values in `fzy1`
    str1 <- match_list[[i]] %>%
      tidyr::unite(
        tidyselect::all_of(fzy1),
        col = "c",
        sep = " ",
        na.rm = TRUE
      ) %>%
      dplyr::pull(c)

    # Merge values in `fzy2`
    str2 <- match_list[[i]] %>%
      tidyr::unite(
        tidyselect::all_of(fzy2),
        col = "c",
        sep = " ",
        na.rm = TRUE
      ) %>%
      dplyr::pull(c)

    # Score similarity of strings in `str1` & `str2`
    match_list[[i]]$sim_score <- stringdist::stringsim(str1, str2)

    match_list[[i]] <- match_list[[i]] %>%
      dplyr::relocate(sim_score) %>%
      dplyr::arrange(dplyr::desc(sim_score))
  }

  as.data.frame(do.call(rbind, match_list))
}
