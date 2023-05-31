#' Assign a sequential ID to distinct rows
#'
#' Assign a sequential ID number to distinct rows in `df` based on selected variables (`var`). The new ID is a string, with the number of characters equal to `nchar(nrow(df))` (if there are 100 rows, the new ID will have three characters).
#'
#' @param df A dataframe.
#' @param var A vector of variable names that the new ID will be based on.
#' @param id_name A name for the new ID variable.
#'
#' @return A dataframe.
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' n_rows <- 20
#' df <- data.frame(
#'   x = sample(c("cat", "horse", "howler monkey"), size = n_rows, replace = TRUE),
#'   y = sample(c(1, 10, 100, NA), size = n_rows, replace = TRUE),
#'   z = rep("ignore this", length.out = n_rows)
#' )
#' df_new <- id_distinct_rows(df, var = c("x", "y"), id_name = "new_id")
#'
id_distinct_rows <- function(df, var, id_name) {
  var_check(df, var = var)

  df_ids <- df %>%
    dplyr::select(dplyr::all_of(var)) %>%
    dplyr::distinct() %>%
    dplyr::mutate({{ id_name }} := formatC(
      x = 1:nrow(.),
      width = nchar(nrow(df)),
      flag = "0"
    ))

  df %>%
    dplyr::left_join(df_ids, by = var)
}
