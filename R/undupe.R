#' Unduplicate a dataframe based on selected variables
#'
#' @description
#' `undupe()` identifies duplicate rows in a dataframe based on a set of variables provided in the arguments. Duplicates are rows that have identical values across this set of variables. All other variables in the dataframe are ignored. `undupe()` returns both a dataframe of distinct rows and a dataframe of duplicates grouped together for easy visual comparison (dupesets).
#'
#' @details
#' When deduplicating a dataframe, one set of variables is "visible" to the process. If rows share identical values across this set of variables, they are considered duplicates. The remaining variables are "invisible" to the deduplication process. Their values are unconstrained within dupesets, therefore a dupeset can have conflicting values within one or more of these invisible variables.
#'
#' `undupe()` allows two methods of setting the visible and invisible variables:
#' * In `visible`, provide the names of variables whose values must match to be evaluated as duplicates.
#' * In `invisible`, provide the names of variables to ignore, in which case all other variables in df become visible during deduplication.
#'
#' Only one of these arguments may be used.
#'
#' `undupe()` adds an identifier to each row based on the unique values in the varibles provided in `visible`. The identifier variable is named `dupe_id` unless another name is provided in `dupe_id_name`. It is added to each dataframe in the output.
#'
#' To produce `df_distinct`, `undupe()` uses [dplyr::distinct()], which returns the first of a set of distinct/unique rows (dupesets) in a dataframe.
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
#'
undupe <- function(df,
                   undupe_vars = NULL,
                   ignore_vars = NULL,
                   dupe_id_name = NULL) {
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
