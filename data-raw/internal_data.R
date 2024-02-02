# Generate internal data for package

md_response_vars <- readRDS("data-raw/md_response_vars.rds")
street_names <- readRDS("data-raw/street_names.rds")
street_suffixes <- readRDS("data-raw/street_suffixes.rds")
unit_prefixes <- readRDS("data-raw/unit_prefixes.rds")

usethis::use_data(
  md_response_vars,
  street_names,
  street_suffixes,
  unit_prefixes,
  internal = TRUE,
  overwrite = TRUE
)


#####



# md_response_vars ####

md_response_vars <- c(
  "AddressDeliveryInstallation", "AddressExtras", "AddressHouseNumber", "AddressKey", "AddressLine1", "AddressLine2", "AddressLockBox", "AddressPostDirection", "AddressPreDirection", "AddressPrivateMailboxName", "AddressPrivateMailboxRange", "AddressRouteService", "AddressStreetName", "AddressStreetSuffix", "AddressSuiteName", "AddressSuiteNumber", "AddressTypeCode", "CBSACode", "CBSADivisionCode", "CBSADivisionLevel", "CBSADivisionTitle", "CBSALevel", "CBSATitle", "CarrierRoute", "CensusBlock", "CensusKey", "CensusTract", "City", "CityAbbreviation", "CompanyName", "CongressionalDistrict", "CountryCode", "CountryName", "CountyFIPS", "CountyName", "CountySubdivisionCode", "CountySubdivisionName", "DeliveryIndicator", "DeliveryPointCheckDigit", "DeliveryPointCode", "ElementarySchoolDistrictCode", "ElementarySchoolDistrictName", "EmailAddress", "Latitude", "Longitude", "MelissaAddressKey", "MelissaAddressKeyBase", "NameFull", "PhoneNumber", "PlaceCode", "PlaceName", "PostalCode", "RecordExtras", "RecordID", "Reserved", "Results", "SecondarySchoolDistrictCode", "SecondarySchoolDistrictName", "State", "StateDistrictLower", "StateDistrictUpper", "StateName", "Suite", "UTC", "UnifiedSchoolDistrictCode", "UnifiedSchoolDistrictName", "UrbanizationName", "TotalRecords", "TransmissionReference", "TransmissionResults", "Version"
)

saveRDS(md_response_vars, "data-raw/md_response_vars.rds")



