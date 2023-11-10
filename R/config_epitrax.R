#' Configure the EpiTrax source file
#'
#' This function configures the EpiTrax source file in preparation for data processing. It does the following:
#'
#' * Adds a unique identifier (`record_id_src`) based on the values in the unmodified source file.
#' * Adds a unique row identifier (`row_id_src`).
#' * Formats all date variables as "YYYY-MM-DD".
#' * Reorders the columns according to `epitrax_vars_reordered`.
#'
#' @param df A dataframe.
#' @param var_dates The EpiTrax date variables to format.
#' @param var_order The order for EpiTrax variables in the returned dataframe.
#'
#' @return
#' A list containing two dataframes:
#'
#' * `data` contains all original variables, `row_id_src`, and `record_id_src`.
#' * `keys` contains `row_id_src`, `patient_id`, `patient_record_number`, and `record_id_src`.
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
# @examples
#'
config_epitrax <- function(df, var_dates = epitrax_date_vars, var_order = epitrax_vars_reordered) {
  if (!assertive::is_data.frame(df)) stop("`df` must be a dataframe", call. = FALSE)

  var_check(df, var = var_order)

  vars_other <- colnames(df)[which(!colnames(df) %in% var_order)]

  if (length(vars_other) > 0) {
    m <- paste(
      "Unexpected variable(s):",
      paste(vars_other, collapse = ", "),
      "\nAdd to `var_order` argument."
    )
    stop(m, call. = FALSE)
  }

  # Reorder variables
  df <- df %>%
    dplyr::select(tidyselect::all_of(var_order))

  # Add `record_id_src`, a unique record identifier based on a hash function
  df$record_id_src <- apply(
    X = df,
    MARGIN = 1,
    FUN = digest::digest,
    algo = "md5"
  )

  # Add `row_id_src`, a unique row identifier (sequential)
  df <- df %>%
    dplyr::mutate(row_id_src = formatC(
      x = 1:nrow(.),
      digits = 0,
      width = 6,
      flag = "0"
    ))

  # Format dates
  df[, var_dates] <- lapply(
    X = df[, var_dates],
    FUN = as.Date,
    format = "%Y-%m-%d %H:%M:%S",
    origin = "1970-01-01"
  )

  # Sort by `lab_collection_date` & `lab_id`, and relocate `row_id_src`
  df <- df %>%
    dplyr::arrange(lab_collection_date, lab_id) %>%
    dplyr::relocate(row_id_src)

  message(
    "Configuration complete\n",
    " * `row_id_src` and `record_id_src` added\n",
    " * Date variables formatted\n",
    " * Columns reordered"
  )

  list(
    # `data` contains all variables, reordered
    data = df,
    # `keys` contains ID variables only
    keys = df %>%
      dplyr::select(
        row_id_src,
        patient_id,
        patient_record_number,
        record_id_src) %>%
      dplyr::mutate(timestamp = Sys.time())
  )
}
