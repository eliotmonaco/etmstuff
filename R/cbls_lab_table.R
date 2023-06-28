#' Create a CBLS Lab Results table
#'
#' @param df A dataframe of records prepared for CBLS submission.
#' @param key A dataframe returned by [cbls_table_key()].
#' @param row_id A unique row identifier variable in `df`.
#' @param ref_labs A reference list of classified lab names.
#'
#' @return A dataframe formatted as a Lab Results table per CBLS guidelines.
#' @export
#'
#' @importFrom magrittr %>%
#'
# @examples
#'
cbls_lab_table <- function(df, key, row_id, ref_labs) {

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

  # Add link variables and FILEID
  df_lab <- df %>%
    dplyr::select(tidyselect::all_of(row_id), patient_id) %>%
    dplyr::mutate(FILEID = "LAB")

  # Add basic format variables
  df_lab <- cbind(df_lab, key)

  # CHILD_ID (required)
  df_lab$CHILD_ID <- df$child_registry_id

  # SAMP_DATE (required)
  df_lab$SAMP_DATE <- format.Date(df$lab_collection_date, "%Y%m%d")

  # ADDR_ID (not required)
  df_lab$ADDR_ID <- df$address_registry_id

  # PREGNANT (not required)
  df_lab$PREGNANT <- strrep(" ", 1)

  # BLANK
  df_lab$BLANK <- strrep(" ", 2)

  # LAB_FUND (required)
  df_lab$LAB_FUND <- dplyr::case_when(
    blood_lead_poisoning_form_col_bl_funding_source == "Public" |
      !is.na(df$medicaid_id) ~ 1,                                               # 1 – Public, includes Medicaid
    blood_lead_poisoning_form_col_bl_funding_source == "Private insurance" ~ 2, # 2 – Private insurance
    blood_lead_poisoning_form_col_bl_funding_source == "Parent self-pay" ~ 3,   # 3 – Parent self-pay
    blood_lead_poisoning_form_col_bl_funding_source == "Unknown" ~ 9,           # 9 – Unknown
    blood_lead_poisoning_form_col_bl_funding_source == "Other" ~ 8,             # 8 – Other
    T ~ 9                                                                       # 9 – Unknown
  )

  # SAMP_TYPE (required)
  df_lab$SAMP_TYPE <- dplyr::case_when(
    df$lab_specimen_source == "Blood - venous" |
      df$lab_specimen_source == "Blood" ~ 1,           # 1 – Venous, blood lead
    df$lab_specimen_source == "Blood - capillary" ~ 2, # 2 – Capillary, blood lead
    T ~ 9                                              # 9 – Unknown
  )

  # TEST_RSN (required)
  df_lab$TEST_RSN <- dplyr::case_when(
    df$test_reason == "cap_scrn" |
      df$test_reason == "ven_cfm_i" ~ 1, # 1 – Screening (asymptomatic child without previous elevated level)
    df$test_reason == "ven_cfm_e" ~ 3,   # 3 – Confirmatory test following elevated value by fingerstick
    df$test_reason == "ven_flw" ~ 4,     # 4 – Follow-up, child with confirmed elevated level
    T ~ 9                                # 9 – Unknown/other
  )

  # LAB_TYPE (required)
  df <- df %>%
    dplyr::left_join(ref_labs, by = "lab_name")

  df_lab$LAB_TYPE <- df$lab_type

  # SCRN_SITE (required)
  df_lab$SCRN_SITE <- dplyr::case_when(
    grepl(
      paste0(
        "Susan", "|", "Univ", "|", "Wesley", "|", "Stormo", "|", "Via Chr",
        "|", "providen", "|", "addis", "|", "advent", "|", "affil",
        "|", "atlas", "|", "chc ", "|", "children", "|", "gracemed",
        "|", "ohp ", "|", "memorial", "|", "luke", "|", "cather",
        "|", "\\bst ", "|", "st. ", "|", "swope", "|", "newton",
        "|", "good sa", "|", "mercy", "|", "kvc", "|", "Clara Barton",
        "|", "Regional Hospital", "|", "ATCHISON HOSP", "|", "newman",
        "|", "Mcpherson Hosp", "|", "prairie heal", "|", "prairie star", "|", "Trinity Rock",
        "|", "Central Ks Reg", "|", "Salina Regional", "|", "Salina Health", "|", "Prompt Care",
        "|", "Eckan", "|", "AMS LABORATORIES", "|", "CHILD CARE", "|", "ARCKC",
        "|", "Heartland Programs"
      ),
      df$ordering_facility_name, ignore.case = TRUE
    ) ~ 4,
    grepl(
      paste0(
        "County", "|", "health dep", "|", "Community", "|", "dist", "|", "primary",
        "|", "smpc", "|", "rodgers", "|", "hunter", "|", " co h",
        "|", "irwin", "|", "kickap", "|", "prairie band", "|", "municipal",
        "|", "Wyco", "|", "tribal", "|", "Munson"
      ),
      df$ordering_facility_name, ignore.case = TRUE
    ) ~ 9,
    grepl(
      paste0(
        " MD", "|", " M.D.", "|", " M.D", "|", "family", "|", "System",
        "|", "pediatri", "|", "geriatr", "|", "medical gr", "|", "plains",
        "|", "clinic", "|", "doctor", "|", "cerner", "|", "comcare",
        "|", "Ministries", "|", "Enterprises", "|", "Fam Health", "|", "HEALTHCARE",
        "|", "SASTUN DIREC", "|", "Lucero", "|", "Kansas Pathology", "|", "PARK FAM CARE"
      ),
      df$ordering_facility_name, ignore.case = TRUE
    ) ~ 4,
    grepl(
      paste0(
        "heid", "|", "vinzant", "|", "lacey", "|", "charlene", "|", "Hartvickson", "|", "Truong"
      ),
      df$ordering_facility_name, ignore.case = TRUE
    ) ~ 4,
    grepl(
      paste0(
        "Labette Health", "|", "Hiawatha Com", "|", "City of", "|", "Nemaha Co Comm"
      ),
      df$ordering_facility_name, ignore.case = TRUE
    ) ~ 9,
    TRUE ~ 9
  )

  # METH_ANAZ (required)
  df_lab$METH_ANAZ <- 9 # Unknown

  # METH_LOD (not required)
  df_lab$METH_LOD <- strrep(" ", 6)

  # SAMP_ANAZ_DT (not required)
  lab_test_date <- format.Date(df$lab_test_date, "%Y%m%d")

  df_lab$SAMP_ANAZ_DT <- dplyr::case_when(
    str_detect(lab_test_date, "^\\d{8}$") ~ lab_test_date,
    T ~ strrep(" ", 8)
  )

  # RSLT_RPT_DT (not required)
  df_lab$RSLT_RPT_DT <- strrep(" ", 8)

  # RESULT (required)
  df_lab$RESULT <- stringr::str_pad(
    format(
      round(df$lab_result_number, digits = 2),
      nsmall = 2,
      trim = T
    ),
    width = 6, side = "left", pad = "0"
  )

  # RST_INTPCODE (required)
  df_lab$RST_INTPCODE <- dplyr::case_when(
    is.na(df$lab_result_symbol) ~ 1, # 1 – Equal
    df$lab_result_symbol == "<" ~ 2, # 2 – Less Than
    df$lab_result_symbol == ">" ~ 3  # 3 – Greater Than
  )

  # LAB_LOD (not required)
  df_lab$LAB_LOD <- strrep(" ", 6)

  # LAB_NAME (not required)
  df_lab$LAB_NAME <- substr(
    stringr::str_pad(
      toupper(df$lab_name),
      width = 43,
      side = "right",
      pad = " "
    ), 1, 43
  )

  # LAB_ID (not required)
  df_lab$LAB_ID <- strrep(" ", 11)

  # NPI (not required)
  df_lab$NPI <- strrep(" ", 10)

  df_lab

}
