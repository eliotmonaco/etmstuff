#' Flatten conflicting values for each variable in a dupeset
#'
#' @description
#' `flatten_conflicts()` combines the different values for selected variables (named in `var`) into one cell so that no data is lost as a result of deduplication. `flatten_conflicts()` must be run on either `df_full` or `df_dupesets`, both produced by [undupe()].
#'
#' Each dupeset is essentially flattened into one row, with each variable named in `var` combining all of the formerly separate values from each duplicate into a single string. Only unique values are kept, separated by the character(s) in `sep`. `NA`s and blank/empty strings are omitted.
#'
#' @inheritSection count_conflicts Deduplication
#'
#' @param df A dataframe of dupesets (or the full data set) returned by [undupe()].
#' @param var A character vector of variable names in `df`.
#' @param dupe_id The duplicate ID variable name, which groups all members of a duplicate set.
#' @param sep A string to use as a separator between the aggregated values from a duplicate set.
#'
#' @return A dataframe of aggregated values from each dupeset (row) and variable (column).
#' @export
#'
#' @importFrom future.apply future_mapply
#' @importFrom future plan multisession
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
#' df_aggregated <- flatten_conflicts(undupe[["df_dupesets"]], var = "z", dupe_id = "dupe_id")
#'
flatten_conflicts <- function(df,
                              var,
                              dupe_id,
                              sep = " | ") {
  var_check(df, var = c(var, dupe_id))

  # List of unique `dupe_id` values
  id_unq <- unique(df[[dupe_id]])

  # DF to hold aggregated `var` values
  df_agg <- data.frame(id_unq)
  colnames(df_agg) <- c(dupe_id)

  # Use each value in `seq` (`id_unq`) to aggregate `var` values
  f <- function(seq, var, separator) {
    df_set <- df %>%
      filter(.data[[dupe_id]] == seq)
    values <- na.omit(unique(df_set[[var]]))
    if (is.character(values)) values <- values[values != ""]
    paste0(values, collapse = separator)
  }

  n_cols <- length(var)

  # Change future strategy to multisession (parallel processing). Save original strategy to `oplan` and call on exit.
  oplan <- future::plan(future::multisession)
  on.exit(future::plan(oplan), add = TRUE)

  for (i in 1:n_cols) { # Loop through variables in `var`
    # df_agg[, var[i]] <- mapply(f, seq = id_unq, var = var[i], SIMPLIFY = TRUE)
    df_agg[, var[i]] <- future_mapply(f, seq = id_unq, var = var[i], separator = sep, SIMPLIFY = TRUE)
    message(paste("Column", i, "of", n_cols, "complete"))
  }

  df_agg
}
