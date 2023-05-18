#' Unduplicate a dataframe based on selected variables
#'
#' `undupe()` identifies duplicate rows in a dataframe and returns a deduplicated dataframe containing only distinct rows and a dataframe of duplicate sets. A duplicate set consists of all rows with identical values across selected variables.
#'
#' In `undupe_vars`, provide the names of variables from `df` whose values must match for rows to be considered duplicates. Alternatively, in `ignore_vars`, provide the names of variables from `df` whose values may differ among duplicates. If `ignore_vars` is provided, all other variables in `df` effectively become the list of `undupe_vars`. Both arguments can't be provided. A unique identifier for duplicate rows is added to the output dataframes.
#'
#' `undupe()` uses `dplyr::distinct()`, which returns distinct/unique rows according to the selected variables. In the case of rows identified as duplicates, only the first row that occurs in `df` is returned in `df_distinct`.
#'
#' @param df A dataframe.
#' @param undupe_vars A character vector of variable names in `df` to use as the basis for unduplication.
#' @param ignore_vars A character vector of variable names in `df` to ignore during unduplication.
#' @param id_name A name for the duplicate ID variable. The default name is `dupe_id` if `id_name` is left `NULL`.
#'
#' @return
#' A list containing three dataframes:
#' * `df_full`, the original dataframe, `df`, plus the duplicate ID variable
#' * `df_distinct`, the deduplicated dataframe containing only distinct rows according to the selected variables
#' * `df_dupesets`, a dataframe of full duplicate sets from `df`, including the duplicates retained in `df_distinct` and the removed duplicates
#'
#' @export
#'
#' @importFrom digest digest
#'
#' @family undupe functions
#' @examples
#' n_rows <- 20
#' df <- data.frame(x = sample(c("cat", "horse", "howler monkey"), size = n_rows, replace = TRUE),
#'                  y = sample(c(1, 10, 100, NA), size = n_rows, replace = TRUE),
#'                  z = sample(c(TRUE, FALSE, NA), size = n_rows, replace = TRUE))
#' undupe <- undupe(df, undupe_vars = c("x", "y"))

undupe <- function(df, undupe_vars=NULL, ignore_vars=NULL, id_name=NULL) {

  # If `id_name` isn't provided, use generic name for duplicate ID variable
  if (is.null(id_name)) id_name <- "dupe_id"

  # Check for presence of `id_name` in the dataframe
  if (any(str_detect(colnames(df), id_name))) {
    m <- paste0("`df` already has a variable named `", id_name, "`. Please provide a different value for `id_name`.")
    stop(m, call. = FALSE)
  }

  # Check if vectors containing variable names are both provided or both unprovided
  if ((is.null(undupe_vars) & is.null(ignore_vars)) |
      (!is.null(undupe_vars) & !is.null(ignore_vars))) {
    stop("Either `undupe_vars` or `ignore_vars` is required, but both can't be provided", call. = F)
  }

  var_check(df, var = c(undupe_vars, ignore_vars))

  if (!is.null(ignore_vars)) {
    undupe_vars <- names(df)[!names(df) %in% ignore_vars]
  }

  # Add n_row & dupe_type
  df <- df %>%
    mutate(n_row = row_number(), dupe_type = "")

  # Add duplicate ID
  df[id_name] <- apply(df %>% select({{ undupe_vars }}), 1, digest::digest, algo = "md5")

  # Subset distinct rows
  df_distinct <- df %>%
    distinct(across(all_of(undupe_vars)), .keep_all = T)

  # Subset removed duplicates (df - df_distinct)
  df_dupes <- df %>%
    anti_join(df_distinct, by = "n_row") %>%
    mutate(dupe_type = "removed")

  # Find rows in df_distinct with a match in df_dupes
  df_matches <- df_distinct %>%
    semi_join(df_dupes, by = id_name) %>%
    mutate(dupe_type = "retained")

  # Join dupe sets and sort by undupe_vars
  df_dupesets <- df_matches %>%
    full_join(df_dupes, by = colnames(df_matches)) %>%
    arrange(.data[[id_name]]) %>%
    select(-n_row)

  if (nrow(df_dupes) == 0) {
    message("No duplicates found")
  } else {
    list(df_full = df %>%
           select(-n_row, -dupe_type), # All records
         df_distinct = df_distinct %>%
           select(-n_row, -dupe_type), # Distinct records
         df_dupesets = df_dupesets)    # Duplicated records
  }

}
