#' Unduplicate a dataframe based on selected variables
#'
#' @description
#' `undupe()` identifies duplicate rows in a dataframe based on a set of variables provided in the arguments. Duplicates are rows that have identical values across this set of variables. All other variables in the dataframe are ignored. `undupe()` returns both a dataframe of distinct rows and a dataframe of duplicates grouped together for easy visual comparison (dupesets).
#'
#' @details
#' When deduplicating a dataframe, one set of variables is "visible" to the process. If rows share identical values across this set of variables, they are considered duplicates. The remaining variables are "invisible" to the deduplication process. Their values are unconstrained within dupesets, therefore a dupeset can have conflicting values within one or more of these invisible variables.
#'
#' `undupe()` allows two methods of setting the visible and invisible variables:
#' * In `visible_var`, provide the names of variables whose values must match to be evaluated as duplicates.
#' * In `invisible_var`, provide the names of variables to ignore, in which case all other variables in df become visible during deduplication.
#'
#' Only one of these arguments may be used.
#'
#' `undupe()` adds an identifier in `dupe_id` to each row based on the unique values in the varibles provided in `visible_var`. It also adds an integer in `dupe_order` which sequentially numbers each member of a dupeset.
#'
#' To produce `df_distinct`, `undupe()` uses [dplyr::distinct()], which returns the first of a set of distinct/unique rows (dupesets) in a dataframe.
#'
#' @param df A dataframe.
#' @param visible_var A character vector of variable names from `df` whose values must match for rows to be considered duplicates.
#' @param invisible_var A character vector of variable names from `df` whose values will be ignored during deduplication. All other variables in `df` will be visible during deduplication.
#' @param prefix A string to add as a prefix to the new variable names, ending `"_id"` and `"_order"`. Defaults to `"dupe"`, creating `dupe_id` and `dupe_order`.
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
#' undupe_list <- undupe(df, visible_var = c("x", "y"))
#'
undupe <- function(df, visible_var = NULL, invisible_var = NULL, prefix = "dupe") {
  # Are both `visible_var` & `invisible_var` provided?
  if (!is.null(visible_var) & !is.null(invisible_var)) {
    stop("Only one of `visible_var` or `invisible_var` can be provided", call. = F)
    # Are both `visible_var` & `invisible_var` missing?
  } else if (is.null(visible_var) & is.null(invisible_var)) {
    stop("One of `visible_var` or `invisible_var` must be provided", call. = F)
  }

  id_var <- paste(prefix, "id", sep = "_")
  order_var <- paste(prefix, "order", sep = "_")

  if (any(c(id_var, order_var) %in% colnames(df))) {
    stop("Provide a different `prefix`", call. = FALSE)
  }

  var_check(df, var = c(visible_var, invisible_var))

  # If `invisible_var` is provided, set `visible_var` to be the complement
  if (!is.null(invisible_var)) {
    visible_var <- colnames(df)[!colnames(df) %in% invisible_var]
  }

  # Add `n_row` as unique row ID and to indicate original row order
  df <- df %>%
    dplyr::mutate(n_row = dplyr::row_number())

  # Add duplicate ID variable
  df[id_var] <- apply(
    df %>%
      dplyr::select({{ visible_var }}),
    MARGIN = 1,
    FUN = digest::digest, algo = "md5"
  )

  # Add variable indicating sequential order within dupesets
  df[[order_var]] <- stats::ave(df$n_row, df[[id_var]], FUN = seq_along)

  # Subset distinct rows
  df_distinct <- df %>%
    dplyr::distinct(dplyr::across(tidyselect::all_of(visible_var)), .keep_all = TRUE)

  # Convert duplicate ID to a factor to preserve original order in `group_by()`
  df[[id_var]] <- factor(df[[id_var]], levels = unique(df[[id_var]]))

  # Pull the duplicate IDs associated with duplicates that occur n > 1 times in `df`
  df_dupe_ids <- df %>%
    dplyr::group_by(.data[[id_var]]) %>%
    dplyr::count() %>%
    dplyr::filter(n > 1) %>%
    dplyr::select(-n) %>%
    dplyr::ungroup()

  # Remove extra class attributes added in previous line
  attr(df_dupe_ids, "class") <- "data.frame"

  # Join rows from `df` that match IDs in `df_dupe_ids`
  df_dupesets <- df_dupe_ids %>%
    dplyr::left_join(df, by = id_var, multiple = "all") %>%
    dplyr::relocate(tidyselect::all_of(id_var), .before = tidyselect::all_of(order_var))

  # Convert duplicate IDs back to character
  df[[id_var]] <- as.character(df[[id_var]])
  df_dupesets[[id_var]] <- as.character(df_dupesets[[id_var]])

  if (nrow(df_dupesets) == 0) {
    return(message("No duplicates found"))
    stop()
  }

  list(
    # Original dataframe
    df_full = df %>%
      dplyr::select(-n_row),
    # Distinct rows only
    df_distinct = df_distinct %>%
      dplyr::select(-n_row),
    # Dupesets only
    df_dupesets = df_dupesets %>%
      dplyr::select(-n_row)
  )
}
