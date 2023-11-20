# Generates `epitrax_vars` list



epitrax_vars <- list()

## Ordered variables ####

epitrax_vars[["ordered"]] <- c(
  # Record IDs
  "lab_id", "lab_test_id", "accession_number",
  # Person IDs & info
  "patient_id", "patient_record_number",
  "patient_birth_date", "age_at_event_date",
  "person_last_name", "person_first_name", "person_middle_name",
  # Lab test info
  "lab_collection_date", "lab_result_value", "lab_units",
  "lab_specimen_source", "lab_loinc_code", "lab_test_type",
  "lab_name", "ordering_facility_name", "ordering_clinician",
  # Demographic info
  "patient_birth_sex", "patient_ethnicity", "patient_race",
  "person_country_of_birth", "patient_pregnant",
  # Address at lab collection
  "lab_collection_street", "lab_collection_unit_number", "lab_collection_city",
  "lab_collection_state", "lab_collection_postal_code","lab_collection_county",
  # Jurisdiction
  "patient_jurisdiction_of_investigation",
  # Treatment
  "treatment_name", "treatment_given", "treatment_date",
  # Insurance
  "bl_medicaid_eligible", "medicaid_id",
  "blood_lead_poisoning_form_col_bl_funding_source",
  # Status
  "patient_state_case_status",
  "patient_workflow_state",
  "patient_event_disposition",
  # Investigation dates
  "first_investigation_started_date",
  "lhd_investigation_start_date",
  "first_accepted_by_lhd_date",
  "patient_investigation_completed_lhd_date",
  "last_investigation_completed_lhd_date",
  "last_approved_by_lhd_date",
  "last_routed_to_lhd_date",
  "patient_results_reported_to_lhd",
  "lhd_date_closed",
  # Other addresses
  "address_at_diagnosis_street", "address_at_diagnosis_unit_number", "address_at_diagnosis_city",
  "address_at_diagnosis_state", "address_at_diagnosis_zip", "address_at_diagnosis_county",
  "current_address_street", "current_address_unit_number", "current_address_city",
  "current_address_state", "current_address_zip", "current_address_county",
  # Other dates
  "lab_test_date", "lab_created_at",
  # Person facility
  "person_facility_name", "person_facility_type", "person_facility_visit_type"
)



## Variables in export file ####

epitrax_vars[["export"]] <- c(
  "accession_number", "address_at_diagnosis_city", "address_at_diagnosis_county", "address_at_diagnosis_state", "address_at_diagnosis_street", "address_at_diagnosis_unit_number", "address_at_diagnosis_zip", "age_at_event_date", "bl_medicaid_eligible", "blood_lead_poisoning_form_col_bl_funding_source", "current_address_city", "current_address_county", "current_address_state", "current_address_street", "current_address_unit_number", "current_address_zip", "first_accepted_by_lhd_date", "first_investigation_started_date", "lab_collection_city", "lab_collection_county", "lab_collection_date", "lab_collection_postal_code", "lab_collection_state", "lab_collection_street", "lab_collection_unit_number", "lab_created_at", "lab_id", "lab_loinc_code", "lab_name", "lab_result_value", "lab_specimen_source", "lab_test_date", "lab_test_id", "lab_test_type", "lab_units", "last_approved_by_lhd_date", "last_investigation_completed_lhd_date", "last_routed_to_lhd_date", "lhd_date_closed", "lhd_investigation_start_date", "medicaid_id", "ordering_clinician", "ordering_facility_name", "patient_birth_date", "patient_birth_sex", "patient_ethnicity", "patient_event_disposition", "patient_id", "patient_investigation_completed_lhd_date", "patient_jurisdiction_of_investigation", "patient_pregnant", "patient_race", "patient_record_number", "patient_results_reported_to_lhd", "patient_state_case_status", "patient_workflow_state", "person_country_of_birth", "person_facility_name", "person_facility_type", "person_facility_visit_type", "person_first_name", "person_last_name", "person_middle_name", "treatment_date", "treatment_given", "treatment_name"
)



## Date variables ####

epitrax_vars[["date"]] <- c(
  "patient_birth_date",
  "lab_collection_date",
  "treatment_date",
  "lab_test_date",
  "lab_created_at",
  "first_investigation_started_date",
  "lhd_investigation_start_date",
  "first_accepted_by_lhd_date",
  "patient_investigation_completed_lhd_date",
  "last_investigation_completed_lhd_date",
  "last_approved_by_lhd_date",
  "last_routed_to_lhd_date",
  "patient_results_reported_to_lhd",
  "lhd_date_closed"
)



## Save ####

usethis::use_data(epitrax_vars, overwrite = T)
