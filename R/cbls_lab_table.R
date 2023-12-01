#' Create a CBLS Lab Results table
#'
#' @param df A dataframe of records prepared for CBLS submission.
#' @param key A dataframe returned by [cbls_table_key()].
#' @param row_id A unique row identifier variable in `df`.
#' @param ref_lab_type A reference list of lab names and their classification numbers for the LAB_TYPE column in the Lab Results table.
#' @param ref_scrn_site A reference list of facility names and their classification numbers for the SCRN_SITE column in the Lab Results table.
#'
#' @return A dataframe formatted as a Lab Results table per CBLS guidelines.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
# @examples
#'
cbls_lab_table <- function(df, row_id, key, ref_lab_type, ref_scrn_site) {
  var_check(df, var = c(
    row_id, "patient_id", "age",
    "lab_collection_date", "lab_test_date",
    "lab_result_symbol", "lab_result_number",
    "lab_name", "ordering_facility_name",
    "blood_lead_poisoning_form_col_bl_funding_source",
    "test_reason",
    "address_registry_id",
    "child_registry_id"
  ))

  if (!all(df$age < 6)) {
    stop("`df$age` must be < 6 for all records")
  }

  var_check(key, var = c("ACTION", "QTR", "RPT_YR", "PGMID"))

  # Add link variables and FILEID
  df_lab <- df %>%
    dplyr::select(tidyselect::all_of(row_id), "patient_id") %>%
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
    df$blood_lead_poisoning_form_col_bl_funding_source == "Public" |
      !is.na(df$medicaid_id) ~ 1, # 1 – Public, includes Medicaid
    df$blood_lead_poisoning_form_col_bl_funding_source == "Private insurance" ~ 2, # 2 – Private insurance
    df$blood_lead_poisoning_form_col_bl_funding_source == "Parent self-pay" ~ 3, # 3 – Parent self-pay
    df$blood_lead_poisoning_form_col_bl_funding_source == "Unknown" ~ 9, # 9 – Unknown
    df$blood_lead_poisoning_form_col_bl_funding_source == "Other" ~ 8, # 8 – Other
    TRUE ~ 9 # 9 – Unknown
  )

  # SAMP_TYPE (required)
  df_lab$SAMP_TYPE <- dplyr::case_when(
    df$lab_specimen_source == "Blood - venous" |
      df$lab_specimen_source == "Blood" ~ 1, # 1 – Venous, blood lead
    df$lab_specimen_source == "Blood - capillary" ~ 2, # 2 – Capillary, blood lead
    TRUE ~ 9 # 9 – Unknown
  )

  # TEST_RSN (required)
  df_lab$TEST_RSN <- dplyr::case_when(
    df$test_reason == "cap_scrn" |
      df$test_reason == "ven_cfm_init" ~ 1, # 1 – Screening (asymptomatic child without previous elevated level)
    df$test_reason == "ven_cfm_elev" |
      df$test_reason == "cap_cfm_elev" ~ 3, # 3 – Confirmatory test following elevated value by fingerstick
    df$test_reason == "ven_flw" ~ 4, # 4 – Follow-up, child with confirmed elevated level
    TRUE ~ 9 # 9 – Unknown/other
  )

  # LAB_TYPE (required)

  df_lab_type <- tidyr::separate_wider_delim(
    df[, c(row_id, "lab_name")],
    cols = "lab_name",
    delim = " | ",
    names_sep = "_",
    too_few = "align_start",
    cols_remove = TRUE
  )

  for (i in 2:ncol(df_lab_type)) {
    nm <- colnames(df_lab_type)[i]
    colnames(df_lab_type)[i] <- "lab_name"
    df_lab_type <- df_lab_type %>%
      dplyr::left_join(ref_lab_type, by = "lab_name")
    colnames(df_lab_type)[i] <- nm
  }

  vars_new <- colnames(df_lab_type)[(i + 1):ncol(df_lab_type)]

  f <- function(x) {
    paste(unique(stats::na.omit(x)), collapse = ",")
  }

  df_lab_type$lab_type_unq <- apply(df_lab_type[, vars_new], 1, f)

  df_lab_type$lab_type_unq[df_lab_type$lab_type_unq == ""] <- NA

  df_lab_type <- df_lab_type %>%
    dplyr::mutate(LAB_TYPE = dplyr::case_when(
      grepl(pattern = ",", x = .data$lab_type_unq) ~ 9,
      is.na(.data$lab_type_unq) ~ 9,
      TRUE ~ as.numeric(.data$lab_type_unq)
    ))

  df_lab <- df_lab %>%
    dplyr::left_join(
      df_lab_type %>%
        dplyr::select(tidyselect::all_of(row_id), LAB_TYPE),
      by = row_id
    )

  # SCRN_SITE (required)

  df_scrn_site <- tidyr::separate_wider_delim(
    df[, c(row_id, "ordering_facility_name")],
    cols = "ordering_facility_name",
    delim = " | ",
    names_sep = "_",
    too_few = "align_start",
    cols_remove = TRUE
  )

  for (i in 2:ncol(df_scrn_site)) {
    nm <- colnames(df_scrn_site)[i]
    colnames(df_scrn_site)[i] <- "facility_name"
    df_scrn_site <- df_scrn_site %>%
      dplyr::left_join(ref_scrn_site, by = "facility_name")
    colnames(df_scrn_site)[i] <- nm
  }

  vars_new <- colnames(df_scrn_site)[(i + 1):ncol(df_scrn_site)]

  df_scrn_site$scrn_site_unq <- apply(df_scrn_site[, vars_new], 1, f)

  df_scrn_site$scrn_site_unq[df_scrn_site$scrn_site_unq == ""] <- NA

  df_scrn_site <- df_scrn_site %>%
    dplyr::mutate(SCRN_SITE = dplyr::case_when(
      grepl(pattern = ",", x = .data$scrn_site_unq) ~ 9,
      is.na(.data$scrn_site_unq) ~ 9,
      TRUE ~ as.numeric(.data$scrn_site_unq)
    ))

  df_lab <- df_lab %>%
    dplyr::left_join(
      df_scrn_site %>%
        dplyr::select(tidyselect::all_of(row_id), "SCRN_SITE"),
      by = row_id
    )

  # METH_ANAZ (required)
  df_lab$METH_ANAZ <- 9 # Unknown

  # METH_LOD (not required)
  df_lab$METH_LOD <- "000.00"

  # SAMP_ANAZ_DT (not required)
  lab_test_date <- format.Date(df$lab_test_date, "%Y%m%d")

  df_lab$SAMP_ANAZ_DT <- dplyr::case_when(
    stringr::str_detect(lab_test_date, "^\\d{8}$") ~ lab_test_date,
    TRUE ~ strrep(" ", 8)
  )

  # RSLT_RPT_DT (not required)
  df_lab$RSLT_RPT_DT <- strrep(" ", 8)

  # RESULT (required)
  df_lab$RESULT <- stringr::str_pad(
    format(
      round(df$lab_result_number, digits = 2),
      nsmall = 2,
      trim = TRUE
    ),
    width = 6, side = "left", pad = "0"
  )

  # RST_INTPCODE (required)
  df_lab$RST_INTPCODE <- dplyr::case_when(
    is.na(df$lab_result_symbol) ~ 1, # 1 – Equal
    df$lab_result_symbol == "<" ~ 2, # 2 – Less Than
    df$lab_result_symbol == ">" ~ 3 # 3 – Greater Than
  )

  # LAB_LOD (not required)
  df_lab$LAB_LOD <- "000.00"

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
