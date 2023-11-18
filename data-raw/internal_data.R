# Generate internal data for package

directions <- readRDS("dev_aux/helpers/directions.rds")
directions_cardinal <- readRDS("dev_aux/helpers/directions_cardinal.rds")
directions_ordinal <- readRDS("dev_aux/helpers/directions_ordinal.rds")
epitrax_date_vars <- readRDS("dev_aux/helpers/epitrax_date_vars.rds")
epitrax_vars <- readRDS("dev_aux/helpers/epitrax_vars.rds")
epitrax_vars_reordered <- readRDS("dev_aux/helpers/epitrax_vars_reordered.rds")
ks_locations <- readRDS("dev_aux/helpers/ks_locations.rds")
md_response_vars <- readRDS("dev_aux/helpers/md_response_vars.rds")
regex_pobox <- readRDS("dev_aux/helpers/regex_pobox.rds")
regex_various <- readRDS("dev_aux/helpers/regex_various.rds")
street_names <- readRDS("dev_aux/helpers/street_names.rds")
street_suffixes <- readRDS("dev_aux/helpers/street_suffixes.rds")
styles_css <- readRDS("dev_aux/helpers/styles_css.rds")
unit_prefixes <- readRDS("dev_aux/helpers/unit_prefixes.rds")

usethis::use_data(
  directions,
  directions_cardinal,
  directions_ordinal,
  epitrax_date_vars,
  epitrax_vars,
  epitrax_vars_reordered,
  ks_locations,
  md_response_vars,
  regex_pobox,
  regex_various,
  street_names,
  street_suffixes,
  styles_css,
  unit_prefixes,
  internal = TRUE,
  overwrite = TRUE
)

#####



# md_response_vars ####

md_response_vars <- c(
  "AddressDeliveryInstallation", "AddressExtras", "AddressHouseNumber", "AddressKey", "AddressLine1", "AddressLine2", "AddressLockBox", "AddressPostDirection", "AddressPreDirection", "AddressPrivateMailboxName", "AddressPrivateMailboxRange", "AddressRouteService", "AddressStreetName", "AddressStreetSuffix", "AddressSuiteName", "AddressSuiteNumber", "AddressTypeCode", "CBSACode", "CBSADivisionCode", "CBSADivisionLevel", "CBSADivisionTitle", "CBSALevel", "CBSATitle", "CarrierRoute", "CensusBlock", "CensusKey", "CensusTract", "City", "CityAbbreviation", "CompanyName", "CongressionalDistrict", "CountryCode", "CountryName", "CountyFIPS", "CountyName", "CountySubdivisionCode", "CountySubdivisionName", "DeliveryIndicator", "DeliveryPointCheckDigit", "DeliveryPointCode", "ElementarySchoolDistrictCode", "ElementarySchoolDistrictName", "EmailAddress", "Latitude", "Longitude", "MelissaAddressKey", "MelissaAddressKeyBase", "NameFull", "PhoneNumber", "PlaceCode", "PlaceName", "PostalCode", "RecordExtras", "RecordID", "Reserved", "Results", "SecondarySchoolDistrictCode", "SecondarySchoolDistrictName", "State", "StateDistrictLower", "StateDistrictUpper", "StateName", "Suite", "UTC", "UnifiedSchoolDistrictCode", "UnifiedSchoolDistrictName", "UrbanizationName", "TotalRecords", "TransmissionReference", "TransmissionResults", "Version"
)

saveRDS(md_response_vars, "dev_aux/helpers/md_response_vars.rds")



# epitrax_vars_reordered ####

epitrax_vars_reordered <- c(
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

saveRDS(epitrax_vars_reordered, "dev_aux/helpers/epitrax_vars_reordered.rds")



# epitrax_vars ####

epitrax_vars <- c(
  "accession_number", "address_at_diagnosis_city", "address_at_diagnosis_county", "address_at_diagnosis_state", "address_at_diagnosis_street", "address_at_diagnosis_unit_number", "address_at_diagnosis_zip", "age_at_event_date", "bl_medicaid_eligible", "blood_lead_poisoning_form_col_bl_funding_source", "current_address_city", "current_address_county", "current_address_state", "current_address_street", "current_address_unit_number", "current_address_zip", "first_accepted_by_lhd_date", "first_investigation_started_date", "lab_collection_city", "lab_collection_county", "lab_collection_date", "lab_collection_postal_code", "lab_collection_state", "lab_collection_street", "lab_collection_unit_number", "lab_created_at", "lab_id", "lab_loinc_code", "lab_name", "lab_result_value", "lab_specimen_source", "lab_test_date", "lab_test_id", "lab_test_type", "lab_units", "last_approved_by_lhd_date", "last_investigation_completed_lhd_date", "last_routed_to_lhd_date", "lhd_date_closed", "lhd_investigation_start_date", "medicaid_id", "ordering_clinician", "ordering_facility_name", "patient_birth_date", "patient_birth_sex", "patient_ethnicity", "patient_event_disposition", "patient_id", "patient_investigation_completed_lhd_date", "patient_jurisdiction_of_investigation", "patient_pregnant", "patient_race", "patient_record_number", "patient_results_reported_to_lhd", "patient_state_case_status", "patient_workflow_state", "person_country_of_birth", "person_facility_name", "person_facility_type", "person_facility_visit_type", "person_first_name", "person_last_name", "person_middle_name", "treatment_date", "treatment_given", "treatment_name"
)

saveRDS(epitrax_vars, "dev_aux/helpers/epitrax_vars.rds")



# epitrax_date_vars ####

epitrax_date_vars <- c(
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

saveRDS(epitrax_date_vars, "dev_aux/helpers/epitrax_date_vars.rds")



# Write and update styles.css ####

## Write styles.css
writeLines(
  styles_css,
  con = "dev_aux/helpers/styles.css"
)

## Read updated styles.css
styles_css <- readLines("dev_aux/helpers/styles.css")

## Save as RDS
saveRDS(styles_css, "dev_aux/helpers/styles_css.rds")



# Create ks_locations ####

## Import list copied from PDF to Excel

zips <- openxlsx::read.xlsx("dev_aux/helpers/ZipCodes2018.xlsx")

zips %>%
  filter(nchar(line) == 1)

zips %>%
  filter(str_detect(line, "•"))

zips <- zips %>%
  filter(nchar(line) > 1) %>%
  filter(!str_detect(line, "•")) %>%
  filter(!str_detect(line, "^2018\\s"))

zips$line2 <- zips$line

zips <- zips[-c(112:115),]

zips$line <- str_replace(zips$line, "(?<=[:alpha:]),", "+")
zips$line <- str_replace(zips$line, "\\.{2,}", "|")

zips$city <- str_extract(zips$line, ".*(?=\\+)")
zips$county <- str_extract(zips$line, "(?<=\\+\\s).*(?=\\|)")
zips$zip <- str_extract(zips$line, "(?<=\\|).*")

zips <- zips %>%
  mutate(zip = if_else(
    !str_detect(line, "[:alpha:]"),
    true = line,
    false = zip
  ))

zips <- zips[, -2]

openxlsx::write.xlsx(zips, "dev_aux/helpers/ZipCodes2018_2.xlsx")


## Import list after manual cleaning

zips <- openxlsx::read.xlsx("dev_aux/helpers/ZipCodes2018_2.xlsx")

zips <- zips %>%
  filter(!is.na(city) | !is.na(county) | !is.na(zip)) %>%
  select(-line)


## Convert zip code strings to columns

extract_seq <- function(x) {
  if (str_detect(x, "-")) {
    as.numeric(substr(x, 1, 5)):as.numeric(substr(x, 7, 11))
  } else {
    as.numeric(x)
  }
}

str_to_num <- function(s) {
  v <- unlist(strsplit(s, split = ", "))
  sort(unlist(lapply(v, extract_seq)))
}

zip_list <- list()

for (i in 1:nrow(zips)) {
  zip_list[[i]] <- data.frame(
    city = zips$city[i],
    county = zips$county[i],
    zip = str_to_num(zips$zip[i])
  )
}

ks_locations <- as.data.frame(do.call(rbind, zip_list))

ks_locations <- ks_locations %>%
  mutate(city = str_to_title(city))

saveRDS(ks_locations, "dev_aux/helpers/ks_locations.rds")



# Modify ks_fips ####

ks_fips <- read.csv("dev_aux/helpers/fips.csv")
colnames(ks_fips) <- tolower(colnames(ks_fips))
colnames(ks_fips)
colnames(ks_fips) <- c("fips", "st_fips", "cnty_fips", "cnty_code", "state", "county")
ks_fips$cnty_fips <- as.character(substr(ks_fips$fips, 3, 5))
ks_fips$fips <- as.character(ks_fips$fips)
ks_fips$st_fips <- as.character(ks_fips$st_fips)

saveRDS(ks_fips, "dev_aux/helpers/ks_fips.rds")



