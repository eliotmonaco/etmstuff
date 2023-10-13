# EpiTrax variables

etmstuff:::epitrax_vars_reordered

## lab_id
min(nchar(epitrax_raw$lab_id), na.rm = T)
max(nchar(epitrax_raw$lab_id), na.rm = T)

## lab_test_id
min(nchar(epitrax_raw$lab_test_id), na.rm = T)
max(nchar(epitrax_raw$lab_test_id), na.rm = T)

## patient_id
min(nchar(epitrax_raw$patient_id), na.rm = T)
max(nchar(epitrax_raw$patient_id), na.rm = T)

## patient_record_number
min(nchar(epitrax_raw$patient_record_number), na.rm = T)
max(nchar(epitrax_raw$patient_record_number), na.rm = T)

## patient_birth_date
print(epitrax_raw$patient_birth_date)

## person_last_name

## person_first_name

## person_middle_name

## lab_collection_date
print(epitrax_raw$lab_collection_date)

## lab_result_value
print(epitrax_raw$lab_result_value)

## lab_units
sort(unique(epitrax_raw$lab_units))

## lab_specimen_source
sort(unique(epitrax_raw$lab_specimen_source))

## lab_loinc_code
sort(unique(epitrax_raw$lab_loinc_code))

## lab_test_type
sort(unique(epitrax_raw$lab_test_type))

## lab_name

## ordering_facility_name

## ordering_clinician

## patient_birth_sex
sort(unique(epitrax_raw$patient_birth_sex))

## patient_ethnicity
sort(unique(epitrax_raw$patient_ethnicity))

## patient_race
sort(unique(unlist(strsplit(epitrax_raw$patient_race, ";"))))

## person_country_of_birth
sort(unique(epitrax_raw$person_country_of_birth))

## patient_pregnant
sort(unique(epitrax_raw$patient_pregnant))

## lab_collection_street

## lab_collection_unit_number

## lab_collection_city

## lab_collection_state

## lab_collection_postal_code

## lab_collection_county

## patient_jurisdiction_of_investigation
sort(unique(epitrax_raw$patient_jurisdiction_of_investigation))

## treatment_name
sort(unique(epitrax_raw$treatment_name))

## treatment_given
sort(unique(epitrax_raw$treatment_given))

## treatment_date
v <- epitrax_raw$treatment_date
print(v[!is.na(v)])

## bl_medicaid_eligible
sort(unique(epitrax_raw$bl_medicaid_eligible))

## medicaid_id
v <- epitrax_raw$medicaid_id
print(v[!is.na(v)])

## blood_lead_poisoning_form_col_bl_funding_source
sort(unique(epitrax_raw$blood_lead_poisoning_form_col_bl_funding_source))

## patient_state_case_status
sort(unique(epitrax_raw$patient_state_case_status))

## patient_workflow_state
sort(unique(epitrax_raw$patient_workflow_state))

## patient_event_disposition
sort(unique(epitrax_raw$patient_event_disposition))

## first_investigation_started_date
v <- epitrax_raw$first_investigation_started_date
print(v[!is.na(v)])

## lhd_investigation_start_date
v <- epitrax_raw$lhd_investigation_start_date
print(v[!is.na(v)])

## first_accepted_by_lhd_date
v <- epitrax_raw$first_accepted_by_lhd_date
print(v[!is.na(v)])

## patient_investigation_completed_lhd_date
v <- epitrax_raw$patient_investigation_completed_lhd_date
print(v[!is.na(v)])

## last_investigation_completed_lhd_date
v <- epitrax_raw$last_investigation_completed_lhd_date
print(v[!is.na(v)])

## last_approved_by_lhd_date
v <- epitrax_raw$last_approved_by_lhd_date
print(v[!is.na(v)])

## last_routed_to_lhd_date
v <- epitrax_raw$last_routed_to_lhd_date
print(v[!is.na(v)])

## patient_results_reported_to_lhd
v <- epitrax_raw$patient_results_reported_to_lhd
print(v[!is.na(v)])

## lhd_date_closed
v <- epitrax_raw$lhd_date_closed
print(v[!is.na(v)])

## address_at_diagnosis_street

## address_at_diagnosis_unit_number

## address_at_diagnosis_city

## address_at_diagnosis_state

## address_at_diagnosis_zip

## address_at_diagnosis_county

## current_address_street

## current_address_unit_number

## current_address_city

## current_address_state

## current_address_zip

## current_address_county

## lab_test_date
v <- epitrax_raw$lab_test_date
print(v[!is.na(v)])

## lab_created_at
v <- epitrax_raw$lab_created_at
print(v[!is.na(v)])

## person_facility_name
v <- epitrax_raw$person_facility_name
print(v[!is.na(v)])

## person_facility_type
sort(unique(unlist(strsplit(epitrax_raw$person_facility_type, ";"))))

## person_facility_visit_type
sort(unique(epitrax_raw$person_facility_visit_type, ";"))



# Added variables

v <- colnames(data_core)
v[!v %in% etmstuff:::epitrax_vars]

## row_id_src
print(data_core$row_id_src)

## age
print(data_core$age)

## lab_result_clean
print(data_core$lab_result_clean)

## lab_result_symbol
v <- data_core$lab_result_symbol
print(v[!is.na(v)])

## lab_result_number
print(data_core$lab_result_number)

## record_id_src
print(data_core$record_id_src)

## dupe_id
print(data_core$dupe_id)

## dupe_order
print(data_core$dupe_order)






