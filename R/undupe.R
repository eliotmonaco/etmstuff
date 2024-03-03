#' Deduplicate a dataframe
#'
#' @description
#' This function identifies duplicate rows in a dataframe based on a subset of variables provided in the arguments. Duplicates are rows that have identical values across this set of variables.
#'
#' @details
#' `undupe()` adds a duplicate identifier (`dupe_id`) to each row, which is a hash determined by the sequence of values in `var`. It also adds an integer (`dupe_order`) which indicates the original row order of each member in a dupe set.
#'
#' `undupe()` returns a list containing a deduplicated dataframe (equivalent to using [dplyr::distinct()]), a dataframe consisting only of dupe sets grouped together, and a copy of the original dataframe with the duplicate identifier and duplicate order variables added.
#'
#' @param df A dataframe.
#' @param var A vector of variables in `df` whose values must match for rows to be considered duplicates. All other variables will be retained in the output but ignored during deduplication.
#' @param prefix A prefix for the new duplicate ID and duplicate order variable names. Defaults to `"dupe"`, creating `dupe_id` and `dupe_order`.
#'
#' @return
#' A list containing three dataframes:
#' * `distinct`: the deduplicated dataframe.
#' * `dupesets`: a dataframe of grouped duplicate sets.
#' * `full`: the original dataframe, `df`, with the duplicate ID and duplicate order variables added.
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @family undupe functions
#' @examples
#' n_rows <- 20
#'
#' df <- data.frame(
#'   x = sample(c("cat", "horse", "howler monkey"), size = n_rows, replace = TRUE),
#'   y = sample(c(1, 10, 100, NA), size = n_rows, replace = TRUE),
#'   z = sample(c("banana", "carrot", "pickle"), size = n_rows, replace = TRUE)
#' )
#'
#' undp <- undupe(df, var = c("x", "y"))
#'
undupe <- function(df, var, prefix = "dupe") {
  if (!is.data.frame(df)) stop("`df` must be a dataframe")
  if (length(prefix) != 1) stop("`prefix` must have length of 1")

  var_check(df, var = var)

  dupe_id <- paste0(prefix, "_id")
  dupe_order <- paste0(prefix, "_order")

  if (any(c(dupe_id, dupe_order) %in% colnames(df))) {
    stop("Provide a different `prefix`")
  }

  # Add row number to preserve original row order
  df <- df %>%
    tibble::add_column(n_row = 1:nrow(df), .name_repair = "unique")
  n_row <- colnames(df)[ncol(df)]

  # Add duplicate ID variable
  df[dupe_id] <- apply(
    df[, var],
    MARGIN = 1,
    FUN = digest::digest, algo = "md5"
  )

  # Add duplicate order variable (original row order within dupe sets)
  df <- df %>%
    dplyr::mutate({{ dupe_order }} := stats::ave(.data[[n_row]], .data[[dupe_id]], FUN = seq_along)) %>%
    dplyr::select(-tidyselect::all_of(n_row))

  # Subset distinct rows
  df_distinct <- df %>%
    dplyr::distinct(dplyr::across(tidyselect::all_of(var)), .keep_all = TRUE)

  # Pull the duplicate IDs that appear more than once in `df`
  df_dupesets <- df %>%
    # Convert `dupe_id` to factor to preserve original row order for `count()`
    dplyr::mutate({{ dupe_id }} := factor(.data[[dupe_id]], levels = unique(.data[[dupe_id]]))) %>%
    dplyr::group_by(.data[[dupe_id]]) %>%
    dplyr::count() %>%
    dplyr::filter(.data$n > 1) %>%
    dplyr::select(-"n") %>%
    dplyr::ungroup() %>%
    # Convert `dupe_id` back to character
    dplyr::mutate({{ dupe_id }} := as.character(.data[[dupe_id]]))

  # Create `df_dupesets` from join with `df`
  df_dupesets <- df_dupesets %>%
    dplyr::left_join(df, by = dupe_id, multiple = "all") %>%
    dplyr::relocate(tidyselect::all_of(dupe_id), .before = tidyselect::all_of(dupe_order))

  if (nrow(df_dupesets) == 0) {
    return(message("No duplicates were found"))
  }

  list(distinct = df_distinct, dupesets = df_dupesets, full = df)
}
