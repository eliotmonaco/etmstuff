cbls_child_table <- function(df, key, row_id) {

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
    stop("`df$age` must be < 6 for all records", call. = FALSE)
  }

  var_check(key, var = c("ACTION", "QTR", "RPT_YR", "PGMID"))

  df <- df %>%
    distinct(child_registry_id, .keep_all = T)

  # Add link variables and FILEID
  df_chi <- df %>%
    select(tidyselect::all_of(row_id), patient_id) %>%
    mutate(FILEID = "CHI")

  # Add basic format variables
  df_chi <- cbind(df_chi, key)

  # CHILD_ID (required)
  df_chi$CHILD_ID <- df$child_registry_id

  # DOB (required)
  df_chi$DOB <- format.Date(df$patient_birth_date, "%Y%m%d")

  # SEX (required)
  df_chi$SEX <- case_when(
    str_detect(df$patient_birth_sex, regex("Male", ignore_case = T)) ~ 1,   # 1 – Male
    str_detect(df$patient_birth_sex, regex("Female", ignore_case = T)) ~ 2, # 2 – Female
    T ~ 9                                                                   # 9 – Unknown
  )

  # ETHNIC (required)
  df_chi$ETHNIC <- case_when(
    str_detect(df$patient_ethnicity, regex("^Hispanic", ignore_case = T)) ~ 1,     # 1 – Hispanic or Latino
    str_detect(df$patient_ethnicity, regex("^Not Hispanic", ignore_case = T)) ~ 2, # 2 – Not Hispanic or Latino
    T ~ 9                                                                          # 9 – Unknown
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
  df_chi$BIRTH <- case_when(
    str_detect(df$person_country_of_birth, regex("United States", ignore_case = T)) ~ 1, # 1 – U.S.
    str_detect(df$person_country_of_birth, regex("Unknown", ignore_case = T)) |
      is.na(df$person_country_of_birth) ~ 3,                                             # 3 – Unknown
    T ~ 2                                                                                # 2 – Other
  )

  # RACE_AIAN (required)
  df_chi$RACE_AIAN <- case_when(
    str_detect(df$patient_race, regex("American Indian or Alaska Native", ignore_case = T)) ~ 1, # 1 – Yes
    T ~ 2                                                                                        # 2 – No
  )

  # RACE_ASIAN (required)
  df_chi$RACE_ASIAN <- case_when(
    str_detect(df$patient_race, regex("Asian", ignore_case = T)) ~ 1, # 1 – Yes
    T ~ 2                                                             # 2 – No
  )

  # RACE_BLACK (required)
  df_chi$RACE_BLACK <- case_when(
    str_detect(df$patient_race, regex("Black or African American", ignore_case = T)) ~ 1, # 1 – Yes
    T ~ 2                                                                                 # 2 – No
  )

  # RACE_NHOPI (required)
  df_chi$RACE_NHOPI <- case_when(
    str_detect(df$patient_race, regex("Native Hawaiian or Other Pacific Islander", ignore_case = T)) ~ 1, # 1 – Yes
    T ~ 2                                                                                                 # 2 – No
  )

  # RACE_WHITE (required)
  df_chi$RACE_WHITE <- case_when(
    str_detect(df$patient_race, regex("White", ignore_case = T)) ~ 1, # 1 – Yes
    T ~ 2                                                             # 2 – No
  )

  # RACE_OTHER (required)
  df_chi$RACE_OTHER <- case_when(
    str_detect(df$patient_race, regex("Other", ignore_case = T)) ~ 1, # 1 – Yes
    T ~ 2                                                             # 2 – No
  )

  # RACE_RTA (required)
  df_chi$RACE_RTA <- case_when(
    str_detect(df$patient_race, regex("Refused", ignore_case = T)) ~ 1, # 1 – Yes
    T ~ 2                                                               # 2 – No
  )

  # RACE_UNK (required)
  df_chi$RACE_UNK <- case_when(
    str_detect(df$patient_race, regex("Unknown", ignore_case = T)) |
      is.na(df$patient_race) ~ 1,                                    # 1 – Yes
    T ~ 2                                                            # 2 – No
  )

  df_chi

}
