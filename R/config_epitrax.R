#' Configure the raw EpiTrax data file
#'
#' Adds the variables `recno`, a unique identifier for each row, and `hash_value`, a unique identifier for the combination of variables in each row. Formats all date columns.
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

  df$hash_value <- apply(
    X = df,
    MARGIN = 1,
    FUN = digest::digest,
    algo = "md5")

  message("Data wrangling: `hash_value` column added")

  df <- df %>%
    dplyr::mutate(recno = formatC(
      x = 1:nrow(.),
      digits = 0,
      width = 6,
      flag = "0")) %>%
    dplyr::filter(!is.na(patient_record_number))

  message("Data wrangling: `recno` column added")

  df[, date_var] <- lapply(
    X = df[, date_var],
    FUN = as.Date,
    format = "%Y-%m-%d %H:%M:%S",
    origin = "1970-01-01"
  )

  message("Data wrangling: date columns formatted")

  list(
    data = df %>%
      dplyr::select(recno, hash_value, dplyr::everything()),
    keys = df %>%
      dplyr::select(recno, hash_value, patient_record_number) %>%
      dplyr::mutate(timestamp = Sys.time())
  )
}
