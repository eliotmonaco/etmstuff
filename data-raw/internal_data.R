# Generate internal data for package

md_response_vars <- readRDS("data-raw/md_response_vars.rds")

usethis::use_data(
  md_response_vars,
  internal = TRUE,
  overwrite = TRUE
)


# md_response_vars

md_response_vars <- c(
  "AddressDeliveryInstallation", "AddressExtras", "AddressHouseNumber", "AddressKey", "AddressLine1", "AddressLine2", "AddressLockBox", "AddressPostDirection", "AddressPreDirection", "AddressPrivateMailboxName", "AddressPrivateMailboxRange", "AddressRouteService", "AddressStreetName", "AddressStreetSuffix", "AddressSuiteName", "AddressSuiteNumber", "AddressTypeCode", "CBSACode", "CBSADivisionCode", "CBSADivisionLevel", "CBSADivisionTitle", "CBSALevel", "CBSATitle", "CarrierRoute", "CensusBlock", "CensusKey", "CensusTract", "City", "CityAbbreviation", "CompanyName", "CongressionalDistrict", "CountryCode", "CountryName", "CountyFIPS", "CountyName", "CountySubdivisionCode", "CountySubdivisionName", "DeliveryIndicator", "DeliveryPointCheckDigit", "DeliveryPointCode", "ElementarySchoolDistrictCode", "ElementarySchoolDistrictName", "EmailAddress", "Latitude", "Longitude", "MelissaAddressKey", "MelissaAddressKeyBase", "NameFull", "PhoneNumber", "PlaceCode", "PlaceName", "PostalCode", "RecordExtras", "RecordID", "Reserved", "Results", "SecondarySchoolDistrictCode", "SecondarySchoolDistrictName", "State", "StateDistrictLower", "StateDistrictUpper", "StateName", "Suite", "UTC", "UnifiedSchoolDistrictCode", "UnifiedSchoolDistrictName", "UrbanizationName", "TotalRecords", "TransmissionReference", "TransmissionResults", "Version"
)

saveRDS(md_response_vars, "data-raw/md_response_vars.rds")
