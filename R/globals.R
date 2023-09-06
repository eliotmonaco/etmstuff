# Global variable declarations to suppress note in R CMD check, as in "function: no visible binding for global variable `x`"

utils::globalVariables(
  c(
    # EpiTrax variables
    "address_at_diagnosis_city", "address_at_diagnosis_county", "address_at_diagnosis_state", "address_at_diagnosis_street", "address_at_diagnosis_unit_number", "address_at_diagnosis_zip", "bl_medicaid_eligible", "blood_lead_poisoning_form_col_bl_funding_source", "current_address_city", "current_address_county", "current_address_state", "current_address_street", "current_address_unit_number", "current_address_zip", "first_accepted_by_lhd_date", "first_investigation_started_date", "lab_collection_city", "lab_collection_county", "lab_collection_date", "lab_collection_postal_code", "lab_collection_state", "lab_collection_street", "lab_collection_unit_number", "lab_created_at", "lab_id", "lab_loinc_code", "lab_name", "lab_result_value", "lab_specimen_source", "lab_test_date", "lab_test_id", "lab_test_type", "lab_units", "last_approved_by_lhd_date", "last_investigation_completed_lhd_date", "last_routed_to_lhd_date", "lhd_date_closed", "lhd_investigation_start_date", "medicaid_id", "ordering_clinician", "ordering_facility_name", "patient_birth_date", "patient_birth_sex", "patient_ethnicity", "patient_event_disposition", "patient_id", "patient_investigation_completed_lhd_date", "patient_jurisdiction_of_investigation", "patient_pregnant", "patient_race", "patient_record_number", "patient_results_reported_to_lhd", "patient_state_case_status", "patient_workflow_state", "person_country_of_birth", "person_facility_name", "person_facility_type", "person_facility_visit_type", "person_first_name", "person_last_name", "person_middle_name", "treatment_date", "treatment_given", "treatment_name",
    # Variables added to data_core
    "row_id_src", "record_id_src", "age", "lab_result_clean", "lab_result_symbol", "lab_result_number", "dupe_id", "dupe_order", "county_of_residence", "lab_result_elev", "bl_ref_val", "test_reason",
    # cbls_address_table()
    "cnty_fips",
    # cbls_check_table()
    "all_chars", "FILEID", "ADDR_ID", "CITY", "CNTY_FIPS", "ZIP", "STATE", "CENSUS", "RENOVATED", "START_REN", "COMP_REN", "CHILD_ID", "DOB", "SEX", "ETHNIC", "BLANK", "CHELATED", "CHEL_TYPE", "CHEL_FUND", "NPLSZ", "NPLSM", "NPLSO", "NPLSH", "NPLSP", "NPLSC", "BIRTH", "RACE_AIAN", "RACE_ASIAN", "RACE_BLACK", "RACE_NHOPI", "RACE_WHITE", "RACE_OTHER", "RACE_RTA", "RACE_UNK", "DATE_REF", "INSP_COMP", "ABAT_COMP", "YEAR", "OWNERSHIP", "DWELL_TYPE", "PAINT_HAZ", "XRF", "DUST_FLOOR", "FLOOR_MSR", "DUST_SILL", "SILL_MSR", "DUST_WELL", "WELL_MSR", "PAINT", "PAINT_MSR", "SOIL", "WATER", "INDHAZ", "DATE_DUE", "INV_CLOS_RES", "CLEAR_DATE", "CLEAR_RSLT", "SAMP_DATE", "PREGNANT", "LAB_FUND", "SAMP_TYPE", "TEST_RSN", "LAB_TYPE", "SCRN_SITE", "METH_ANAZ", "METH_LOD", "SAMP_ANAZ_DT", "RSLT_RPT_DT", "RESULT", "RST_INTPCODE", "LAB_LOD", "LAB_NAME", "LAB_ID", "NPI",
    # cbls_undupe()
    "cbls_dupe_id", "cbls_dupe_order", "cbls_duplicate",
    # clean_street_address()
    "removed_text", "replacement_text",
    # compare_parsed_street()
    "temp",
    # count_conflicts()
    "n",
    # fuzzy_compare()
    "sim_score",
    # merge_address_registry ()
    "address_registry_id", "lookup",
    # merge_child_registry ()
    "child_registry_id",
    # parse_street_addresses()
    "pm.uid",
    # pick_max_result()
    "pid_unq",
    # pull_addresses()
    "street_src", "street", "unit", "city", "state", "zip", "county",
    # simulate_data()
    "address_id",
    # validate_address()
    "n_row",
    ".", "i"
  )
)
