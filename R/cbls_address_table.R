# `df` = data_core_clppp (the DF of all records included in the CLPPP submission)
# `address_registry` = df_addresses (the DF of address_registry from the submission quarter with `address_registry_id`s)

cbls_address_table <- function(df, key, address_registry) {

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
    filter(address_registry_id != "00000000")

  address_registry <- address_registry %>%
    distinct(address_registry_id, .keep_all = T) %>%
    mutate(census_tract = "")

  df_add <- df %>%
    select(address_registry_id) %>%
    distinct() %>%
    left_join(address_registry, by = "address_registry_id") %>%
    select(
      ADDR_ID = address_registry_id,
      CITY = city,
      CNTY_FIPS = FIPS,
      ZIP = zip,
      STATE = state,
      CENSUS = census_tract
    ) %>%
    mutate(
      FILEID = "ADD",
      CITY = substr(
        str_pad(
          toupper(CITY),
          width = 15,
          side = "right",
          pad = " "
        ), 1, 15
      ),
      # CNTY_FIPS = substr(CNTY_FIPS, 3, 5),
      ZIP = str_pad(
        sub("-", "", ZIP),
        width = 9,
        side = "right",
        pad = " "
      ),
      CENSUS = str_pad(
        CENSUS,
        width = 7,
        side = "right",
        pad = " "
      )
    )

  # Add basic format variables
  df_add <- cbind(key, df_add)

  # RENOVATED (required)
  df_add$RENOVATED <- 9 # Unknown

  # START_REN (not required)
  df_add$START_REN <- strrep(" ", 8)

  # COMP_REN (not required)
  df_add$COMP_REN <- strrep(" ", 8)

  df_add %>%
    relocate(FILEID)

}
