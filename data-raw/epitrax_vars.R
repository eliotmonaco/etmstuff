# Generates `epitrax_vars.rda`



epitrax_vars <- list()

## All exported variables
epitrax_vars$all <- c(
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
  # Jurisdiction & case status
  "patient_jurisdiction_of_investigation", "patient_state_case_status",
  # Treatment
  "treatment_name", "treatment_given", "treatment_date",
  # Funding
  "blood_lead_poisoning_form_col_bl_funding_source",
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

## Date variables
epitrax_vars$date <- c(
  "patient_birth_date",
  "lab_collection_date"
)

## Variables kept in final data set
epitrax_vars$final <- c(
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
  # Jurisdiction & case status
  "patient_jurisdiction_of_investigation", "patient_state_case_status",
  # Treatment
  "treatment_name", "treatment_given", "treatment_date",
  # Funding
  "blood_lead_poisoning_form_col_bl_funding_source",
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

usethis::use_data(epitrax_vars, overwrite = T)
