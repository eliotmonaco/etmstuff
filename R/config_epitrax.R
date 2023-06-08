#' Configure the raw EpiTrax data file
#'
#' Adds the variables `src_row_id`, a unique identifier for each row, and `src_record_id`, a unique identifier for the combination of variables in each row. Formats all date columns.
#'
#' @param df A dataframe.
#'
#' @return A list containing two dataframes: `data` and `keys`.
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

  df <- df %>%
    dplyr::mutate(src_row_id = formatC(
      x = 1:nrow(.),
      digits = 0,
      width = 6,
      flag = "0"
    ))

  df$src_record_id <- apply(
    X = df,
    MARGIN = 1,
    FUN = digest::digest,
    algo = "md5"
  )

  df[, date_var] <- lapply(
    X = df[, date_var],
    FUN = as.Date,
    format = "%Y-%m-%d %H:%M:%S",
    origin = "1970-01-01"
  )

  message(
    "Configuration complete\n",
    " - `src_row_id` added\n",
    " - `src_record_id` added\n",
    " - Date variables formatted\n",
    " - Columns reordered"
  )

  list(
    data = df %>%
      dplyr::select(src_row_id, tidyselect::all_of(epitrax_variables_reordered), src_record_id),
    keys = df %>%
      dplyr::select(src_row_id, patient_id, patient_record_number, src_record_id) %>%
      dplyr::mutate(timestamp = Sys.time())
  )
}
