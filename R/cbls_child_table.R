#' Create a CBLS Child table
#'
#' @param df A dataframe of records prepared for CBLS submission.
#' @param key A dataframe returned by [cbls_table_key()].
#' @param row_id A unique row identifier variable in `df`.
#'
#' @return A dataframe formatted as a Child table per CBLS guidelines.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
# @examples
#'
cbls_child_table <- function(df, row_id, key) {
  var_check(df, var = c(
    row_id, "patient_id", "age",
    "lab_collection_date", "lab_test_date",
    "lab_result_symbol", "lab_result_number",
    "lab_name", "ordering_facility_name",
    "blood_lead_poisoning_form_col_bl_funding_source",
    "address_registry_id",
    "child_registry_id",
    "test_reason"
  ))

  if (!all(df$age < 6)) {
    stop("`df$age` must be < 6 for all records")
  }

  var_check(key, var = c("ACTION", "QTR", "RPT_YR", "PGMID"))

  df <- df %>%
    dplyr::distinct(.data$child_registry_id, .keep_all = TRUE)

  # Add link variables, FILEID, and `key`
  df_chi <- df %>%
    dplyr::select(tidyselect::all_of(row_id), "patient_id") %>%
    dplyr::mutate(FILEID = "CHI") %>%
    dplyr::bind_cols(key)

  # CHILD_ID (required)
  df_chi$CHILD_ID <- df$child_registry_id

  # DOB (required)
  df_chi$DOB <- format.Date(df$patient_birth_date, "%Y%m%d")

  # SEX (required)
  df_chi$SEX <- dplyr::case_when(
    df$patient_birth_sex == "Male" ~ 1, # 1 – Male
    df$patient_birth_sex == "Female" ~ 2, # 2 – Female
    TRUE ~ 9 # 9 – Unknown
  )

  # ETHNIC (required)
  df_chi$ETHNIC <- dplyr::case_when(
    df$patient_ethnicity == "Hispanic or Latino" ~ 1, # 1 – Hispanic or Latino
    df$patient_ethnicity == "Not Hispanic or Latino" ~ 2, # 2 – Not Hispanic or Latino
    TRUE ~ 9 # 9 – Unknown
  )

  # BLANK
  df_chi$BLANK <- strrep(" ", 1)

  # CHELATED (required)
  df_chi$CHELATED <- 2 # No

  # CHEL_TYPE (required)
  df_chi$CHEL_TYPE <- strrep(" ", 1)

  # CHEL_FUND (required)
  df_chi$CHEL_FUND <- strrep(" ", 1)

  # NPLSZ (required)
  df_chi$NPLSZ <- 9 # Unknown

  # NPLSM (required)
  df_chi$NPLSM <- 9 # Unknown

  # NPLSO (required)
  df_chi$NPLSO <- 9 # Unknown

  # NPLSH (required)
  df_chi$NPLSH <- 9 # Unknown

  # NPLSP (required)
  df_chi$NPLSP <- 9 # Unknown

  # NPLSC (required)
  df_chi$NPLSC <- 9 # Unknown

  # BIRTH (not required)
  df_chi$BIRTH <- dplyr::case_when(
    df$person_country_of_birth == "United States" ~ 1, # 1 – U.S.
    df$person_country_of_birth == "Unknown" |
      is.na(df$person_country_of_birth) ~ 3, # 3 – Unknown
    TRUE ~ 2 # 2 – Other
  )

  # RACE_AIAN (required)
  p <- "American Indian or Alaska Native"
  df_chi$RACE_AIAN <- dplyr::case_when(
    stringr::str_detect(df$patient_race, stringr::regex(p, ignore_case = TRUE)) ~ 1, # 1 – Yes
    TRUE ~ 2 # 2 – No
  )

  # RACE_ASIAN (required)
  p <- "Asian"
  df_chi$RACE_ASIAN <- dplyr::case_when(
    stringr::str_detect(df$patient_race, stringr::regex(p, ignore_case = TRUE)) ~ 1, # 1 – Yes
    TRUE ~ 2 # 2 – No
  )

  # RACE_BLACK (required)
  p <- "Black or African American"
  df_chi$RACE_BLACK <- dplyr::case_when(
    stringr::str_detect(df$patient_race, stringr::regex(p, ignore_case = TRUE)) ~ 1, # 1 – Yes
    TRUE ~ 2 # 2 – No
  )

  # RACE_NHOPI (required)
  p <- "Native Hawaiian or Other Pacific Islander"
  df_chi$RACE_NHOPI <- dplyr::case_when(
    stringr::str_detect(df$patient_race, stringr::regex(p, ignore_case = TRUE)) ~ 1, # 1 – Yes
    TRUE ~ 2 # 2 – No
  )

  # RACE_WHITE (required)
  p <- "White"
  df_chi$RACE_WHITE <- dplyr::case_when(
    stringr::str_detect(df$patient_race, stringr::regex(p, ignore_case = TRUE)) ~ 1, # 1 – Yes
    TRUE ~ 2 # 2 – No
  )

  # RACE_OTHER (required)
  p <- "Other"
  df_chi$RACE_OTHER <- dplyr::case_when(
    stringr::str_detect(df$patient_race, stringr::regex(p, ignore_case = TRUE)) ~ 1, # 1 – Yes
    TRUE ~ 2 # 2 – No
  )

  # RACE_RTA (required)
  p <- "Refused"
  df_chi$RACE_RTA <- dplyr::case_when(
    stringr::str_detect(df$patient_race, stringr::regex(p, ignore_case = TRUE)) ~ 1, # 1 – Yes
    TRUE ~ 2 # 2 – No
  )

  # RACE_UNK (required)
  p <- "Unknown"
  df_chi$RACE_UNK <- dplyr::case_when(
    stringr::str_detect(df$patient_race, stringr::regex(p, ignore_case = TRUE)) |
      is.na(df$patient_race) ~ 1, # 1 – Yes
    TRUE ~ 2 # 2 – No
  )

  df_chi
}
