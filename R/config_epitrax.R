#' Configure the EpiTrax source file
#'
#' This function configures the EpiTrax source file in preparation for data processing. It does the following:
#'
#' * Adds a unique identifier (`record_id_src`) based on the values in the unmodified source file.
#' * Adds a unique row identifier (`row_id_src`).
#' * Formats all date variables as "YYYY-MM-DD".
#' * Reorders the columns according to `epitrax_variables_reordered`.
#'
#' @param df A dataframe.
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
config_epitrax <- function(df) {
  if (!assertive::is_data.frame(df)) stop("`df` must be a dataframe", call. = FALSE)

  date_var <- c(
    "patient_birth_date",
    "treatment_date",
    "lab_collection_date",
    "lab_test_date",
    "lab_created_at",
    "patient_investigation_completed_lhd_date",
    "lhd_investigation_start_date",
    "lhd_date_closed",
    "first_investigation_started_date",
    "last_investigation_completed_lhd_date",
    "first_accepted_by_lhd_date",
    "last_approved_by_lhd_date",
    "last_routed_to_lhd_date",
    "patient_results_reported_to_LHD"
  )

  var_check(df, var = date_var)

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
  df[, date_var] <- lapply(
    X = df[, date_var],
    FUN = as.Date,
    format = "%Y-%m-%d %H:%M:%S",
    origin = "1970-01-01"
  )

  message(
    "Configuration complete\n",
    " - `record_id_src` added\n",
    " - `row_id_src` added\n",
    " - Date variables formatted\n",
    " - Columns reordered"
  )

  list(
    # `data` contains all variables, reordered
    data = df %>%
      dplyr::select(
        row_id_src,
        tidyselect::all_of(epitrax_variables_reordered),
        record_id_src),
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
