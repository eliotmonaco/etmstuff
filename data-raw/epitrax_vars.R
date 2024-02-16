# Generates `epitrax_vars.rda`



epitrax_vars <- list()

## All exported variables
epitrax_vars$all <- c(
  # Person
  "patient_id", "patient_record_number",
  "person_last_name", "person_first_name", "person_middle_name",
  "patient_birth_date", "age_at_event_date",
  # Lab test
  "lab_name", "ordering_facility_name", "ordering_clinician",
  "lab_collection_date", "lab_specimen_source", "accession_number",
  "lab_test_type", "lab_result_value", "lab_units",
  "lab_loinc_code", "lab_id", "lab_test_id",
  # Demographics
  "patient_birth_sex", "blood_lead_poisoning_form_col_bl_funding_source",
  "patient_ethnicity", "patient_race", "person_country_of_birth",
  # Case
  "patient_jurisdiction_of_investigation", "patient_state_case_status",
  # Clinical
  "patient_pregnant",
  "treatment_given", "treatment_date", "treatment_name",
  # Addresses
  "lab_collection_street", "lab_collection_unit_number", "lab_collection_city",
  "lab_collection_state", "lab_collection_postal_code","lab_collection_county",
  "address_at_diagnosis_street", "address_at_diagnosis_unit_number", "address_at_diagnosis_city",
  "address_at_diagnosis_state", "address_at_diagnosis_zip", "address_at_diagnosis_county",
  "current_address_street", "current_address_unit_number", "current_address_city",
  "current_address_state", "current_address_zip", "current_address_county",
  # Extra
  "lab_test_date", "lab_created_at",
  "person_facility_name", "person_facility_type", "person_facility_visit_type"
)

## Variables kept in final data set
epitrax_vars$final <- c(
  # Person
  "patient_id", "patient_record_number",
  "person_last_name", "person_first_name", "person_middle_name",
  "patient_birth_date", "age_at_event_date",
  # Lab test
  "lab_name", "ordering_facility_name", "ordering_clinician",
  "lab_collection_date", "lab_specimen_source", "accession_number",
  "lab_test_type", "lab_result_value", "lab_units",
  "lab_loinc_code", "lab_id", "lab_test_id",
  # Demographics
  "patient_birth_sex", "blood_lead_poisoning_form_col_bl_funding_source",
  "patient_ethnicity", "patient_race", "person_country_of_birth",
  # Case
  "patient_jurisdiction_of_investigation", "patient_state_case_status",
  # Clinical
  "patient_pregnant",
  "treatment_given", "treatment_date", "treatment_name",
  # Addresses
  "lab_collection_street", "lab_collection_unit_number", "lab_collection_city",
  "lab_collection_state", "lab_collection_postal_code","lab_collection_county",
  "address_at_diagnosis_street", "address_at_diagnosis_unit_number", "address_at_diagnosis_city",
  "address_at_diagnosis_state", "address_at_diagnosis_zip", "address_at_diagnosis_county",
  "current_address_street", "current_address_unit_number", "current_address_city",
  "current_address_state", "current_address_zip", "current_address_county"
)

usethis::use_data(epitrax_vars, overwrite = T)
