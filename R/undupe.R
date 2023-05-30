#' Unduplicate a dataframe based on selected variables
#'
#' @description
#' `undupe()` identifies duplicate rows in a dataframe based on a set of variables provided in the arguments. Duplicates are rows that have identical values across this set of variables. All other variables in the dataframe are ignored. `undupe()` returns both a dataframe of distinct rows and a dataframe of duplicates grouped together for easy visual comparison (dupesets).
#'
#' @details
#' When deduplicating a dataframe, one set of variables is "visible" to the process. If rows share identical values across this set of variables, they are considered duplicates. The remaining variables are "invisible" to the deduplication process. Their values are unconstrained within dupesets, therefore a dupeset can have conflicting values within one or more of these invisible variables.
#'
#' `undupe()` allows two methods of setting the visible and invisible variables:
#' * In `visible_vars`, provide the names of variables whose values must match to be evaluated as duplicates.
#' * In `invisible_vars`, provide the names of variables to ignore, in which case all other variables in df become visible during deduplication.
#'
#' Only one of these arguments may be used.
#'
#' `undupe()` adds an identifier in `dupe_id` to each row based on the unique values in the varibles provided in `visible_vars`. It also adds an integer in `dupe_order` which sequentially numbers each member of a dupeset.
#'
#' To produce `df_distinct`, `undupe()` uses [dplyr::distinct()], which returns the first of a set of distinct/unique rows (dupesets) in a dataframe.
#'
#' @param df A dataframe.
#' @param visible_vars A character vector of variable names from `df` whose values must match for rows to be considered duplicates.
#' @param invisible_vars A character vector of variable names from `df` whose values will be ignored during deduplication. All other variables in `df` will be visible during deduplication.
#'
#' @return
#' A list containing three dataframes:
#' * `df_full`: The original dataframe, `df`, with the duplicate ID variable added.
#' * `df_distinct`: The deduplicated dataframe, which contains only distinct rows from `df` according to the selected variables.
#' * `df_dupesets`: A dataframe of grouped duplicate sets from `df`, with each set consisting of the duplicate retained in `df_distinct` and the duplicate(s) removed. Rows in `df` without duplicates are not included.
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @family undupe functions
#' @examples
#' n_rows <- 20
#' df <- data.frame(
#'   x = sample(c("cat", "horse", "howler monkey"), size = n_rows, replace = TRUE),
#'   y = sample(c(1, 10, 100, NA), size = n_rows, replace = TRUE),
#'   z = sample(c("banana", "carrot", "pickle"), size = n_rows, replace = TRUE)
#' )
#' undupe_list <- undupe(df, visible_vars = c("x", "y"))
#'
undupe <- function(df, visible_vars = NULL, invisible_vars = NULL) {
  # Are both `visible_vars` & `invisible_vars` provided?
  if (!is.null(visible_vars) & !is.null(invisible_vars)) {
    stop("Only one of `visible_vars` or `invisible_vars` can be supplied", call. = F)
    # Are both `visible_vars` & `invisible_vars` missing?
  } else if (is.null(visible_vars) & is.null(invisible_vars)) {
    stop("One of `visible_vars` or `invisible_vars` must be supplied", call. = F)
  }

  var_check(df, var = c(visible_vars, invisible_vars))

  # If `invisible_vars` is provided, set `visible_vars` to be the complement
  if (!is.null(invisible_vars)) {
    visible_vars <- colnames(df)[!colnames(df) %in% invisible_vars]
  }

  # Add `n_row` as unique row ID and to indicate original row order
  df <- df %>%
    dplyr::mutate(n_row = row_number())

  # Add duplicate ID variable
  df["dupe_id"] <- apply(
    df %>% dplyr::select({{ visible_vars }}),
    MARGIN = 1,
    FUN = digest::digest, algo = "md5"
  )

  # Add variable indicating sequential order within dupesets
  df$dupe_order <- stats::ave(df$n_row, df$dupe_id, FUN = seq_along)

  # Subset distinct rows
  df_distinct <- df %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(visible_vars)), .keep_all = TRUE)

  # Convert duplicate ID to a factor to preserve original order in `group_by()`
  df$dupe_id <- factor(df$dupe_id, levels = unique(df$dupe_id))

  # Pull the duplicate IDs associated with duplicates that occur n > 1 times in `df`
  df_dupe_ids <- df %>%
    dplyr::group_by(dupe_id) %>%
    dplyr::count() %>%
    dplyr::filter(n > 1) %>%
    dplyr::select(-n) %>%
    dplyr::ungroup()

  # Remove extra class attributes added in previous line
  attr(df_dupe_ids, "class") <- "data.frame"

  # Join rows from `df` that match IDs in `df_dupe_ids`
  df_dupesets <- df_dupe_ids %>%
    dplyr::left_join(df, by = "dupe_id", multiple = "all") %>%
    dplyr::relocate(dupe_id, .before = dupe_order)

  # Convert duplicate IDs back to character
  df$dupe_id <- as.character(df$dupe_id)
  df_dupesets$dupe_id <- as.character(df_dupesets$dupe_id)

  if (nrow(df_dupesets) == 0) {
    return(message("No duplicates found"))
    stop()
  }

  list(
    # Original dataframe
    df_full = df,
    # Distinct rows only
    df_distinct = df_distinct,
    # Dupesets only
    df_dupesets = df_dupesets
  )
}
