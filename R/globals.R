# Global variable declarations to suppress note in R CMD check, as in "function: no visible binding for global variable `x`"

utils::globalVariables(
  c(
    # EpiTrax variables
    "patient_record_number", "patient_id", "person_last_name", "person_first_name",
    "person_middle_name", "patient_birth_date", "patient_birth_sex", "patient_ethnicity",
    "person_country_of_birth", "patient_race", "current_address_street",
    "current_address_unit_number", "current_address_city", "current_address_county",
    "current_address_state", "current_address_zip", "address_at_diagnosis_street",
    "address_at_diagnosis_unit_number", "address_at_diagnosis_city",
    "address_at_diagnosis_state", "address_at_diagnosis_county", "address_at_diagnosis_zip",
    "person_facility_name", "person_facility_type", "person_facility_visit_type",
    "patient_pregnant", "treatment_name", "treatment_given", "treatment_date", "lab_name",
    "lab_result_value", "lab_units", "lab_specimen_source", "lab_collection_date",
    "lab_test_date", "lab_collection_street", "lab_collection_unit_number",
    "lab_collection_city", "lab_collection_county", "lab_collection_state",
    "lab_collection_postal_code", "bl_medicaid_eligible", "medicaid_id",
    "patient_state_case_status", "patient_jurisdiction_of_investigation", "lab_created_at",
    "blood_lead_poisoning_form_col_bl_funding_source", "ordering_clinician",
    "ordering_facility_name", "lab_test_type", "lab_loinc_code", "person_country_of_birth-2",
    "patient_workflow_state", "patient_investigation_completed_lhd_date",
    "lhd_investigation_start_date", "lhd_date_closed", "patient_event_disposition",
    "first_investigation_started_date", "last_investigation_completed_lhd_date",
    "first_accepted_by_lhd_date", "last_approved_by_lhd_date", "last_routed_to_lhd_date",
    "patient_results_reported_to_LHD",
    # clean_lab_results()
    "lab_result_clean",
    # clean_values()
    "removed_text", "replacement_text",
    # compare_parsed_street()
    "temp",
    # config_epitrax()
    "row_id_src", "record_id_src",
    # count_conflicts()
    "n",
    # created by parse_lab_results()
    "lab_result_symbol", "lab_result_number",
    # pull_addresses()
    "street", "unit", "city", "state", "zip", "county",
    # simulate_data()
    "address_id",
    # undupe()
    "dupe_id", "dupe_order",
    # validate_values()
    "n_row"
  )
)
