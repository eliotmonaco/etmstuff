#' Unduplicate a dataframe based on selected variables
#'
#' `undupe()` identifies duplicate rows in a dataframe and returns a deduplicated dataframe containing only distinct rows and a dataframe of duplicate sets (dupesets). A dupeset consists of all rows with identical values across selected variables.
#'
#' In `undupe_vars`, provide the names of variables from `df` whose values must match for rows to be considered duplicates. Alternatively, in `ignore_vars`, provide the names of variables from `df` whose values may differ among duplicates. If `ignore_vars` is provided, all other variables in `df` effectively become the list of `undupe_vars`. Both arguments can't be provided. A unique identifier for duplicate rows (named `dupe_id` by default) is added to the output dataframes.
#'
#' `undupe()` uses `dplyr::distinct()`, which returns distinct/unique rows according to the selected variables. In the case of rows identified as duplicates, only the first row that occurs in `df` is returned in `df_distinct`.
#'
#' @param df A dataframe.
#' @param undupe_vars A character vector of variable names in `df` to use as the basis for unduplication.
#' @param ignore_vars A character vector of variable names in `df` to ignore during unduplication.
#' @param dupe_id_name A name for the duplicate ID variable. The default name is `dupe_id` if `dupe_id_name` is left `NULL`.
#'
#' @return
#' A list containing three dataframes:
#' * `df_full`: The original dataframe, `df`, with the duplicate ID variable added.
#' * `df_distinct`: The deduplicated dataframe, which contains only distinct rows from `df` according to the selected variables.
#' * `df_dupesets`: A dataframe of grouped duplicate sets from `df`, with each set consisting of the duplicate retained in `df_distinct` and the duplicate(s) removed. Rows in `df` without duplicates are not included.
#'
#' @export
#'
#' @importFrom digest digest
#'
#' @family undupe functions
#' @examples
#' n_rows <- 20
#' df <- data.frame(
#'   x = sample(c("cat", "horse", "howler monkey"), size = n_rows, replace = TRUE),
#'   y = sample(c(1, 10, 100, NA), size = n_rows, replace = TRUE),
#'   z = sample(c("banana", "carrot", "pickle"), size = n_rows, replace = TRUE)
#' )
#' undupe <- undupe(df, undupe_vars = c("x", "y"))
undupe <- function(df, undupe_vars = NULL, ignore_vars = NULL, dupe_id_name = NULL) {
  # If `dupe_id_name` isn't provided, use "dupe_id" as the default
  if (is.null(dupe_id_name)) dupe_id_name <- "dupe_id"

  # Check for presence of `dupe_id_name` in the dataframe
  if (any(str_detect(colnames(df), dupe_id_name))) {
    m <- paste0(
      "`df` already has a variable named `", dupe_id_name,
      "`. Please provide a different value for `dupe_id_name`."
    )
    stop(m, call. = FALSE)
  }

  # Check if vectors containing variable names are both provided or both unprovided
  if (!is.null(undupe_vars) & !is.null(ignore_vars)) {
    stop("Only one of `undupe_vars` or `ignore_vars` can be supplied", call. = F)
  } else if (is.null(undupe_vars) & is.null(ignore_vars)) {
    stop("One of `undupe_vars` or `ignore_vars` must be supplied", call. = F)
  }

  var_check(df, var = c(undupe_vars, ignore_vars))

  if (!is.null(ignore_vars)) {
    undupe_vars <- names(df)[!names(df) %in% ignore_vars]
  }

  # Add `n_row` & `dupe_type`
  df <- df %>%
    mutate(n_row = row_number(), dupe_type = "")

  # Add duplicate ID
  df[dupe_id_name] <- apply(df %>% select({{ undupe_vars }}), 1, digest::digest, algo = "md5")

  # Subset distinct rows
  df_distinct <- df %>%
    distinct(across(all_of(undupe_vars)), .keep_all = TRUE)

  # Subset removed duplicates (`df` - `df_distinct`)
  df_dupes <- df %>%
    anti_join(df_distinct, by = "n_row") %>%
    mutate(dupe_type = "removed")

  # Find rows in `df_distinct` with a match in `df_dupes`
  df_matches <- df_distinct %>%
    semi_join(df_dupes, by = dupe_id_name) %>%
    mutate(dupe_type = "retained")

  # Join dupesets and sort by `undupe_vars`
  df_dupesets <- df_matches %>%
    full_join(df_dupes, by = colnames(df_matches)) %>%
    arrange(.data[[dupe_id_name]]) %>%
    select(-n_row)

  if (nrow(df_dupes) == 0) {
    message("No duplicates found")
  } else {
    list(
      # Original records, plus duplicate ID
      df_full = df %>%
        select(-n_row, -dupe_type),
      # Distinct records only
      df_distinct = df_distinct %>%
        select(-n_row, -dupe_type),
      # Dupesets
      df_dupesets = df_dupesets
    )
  }
}
