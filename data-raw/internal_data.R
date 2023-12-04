# Generate internal data for package

directions <- readRDS("data-raw/directions.rds")
# directions_cardinal <- readRDS("dev_aux/helpers/directions_cardinal.rds")
# directions_ordinal <- readRDS("dev_aux/helpers/directions_ordinal.rds")
ks_locations <- readRDS("data-raw/ks_locations.rds")
md_response_vars <- readRDS("data-raw/md_response_vars.rds")
md_summarize <- readRDS("data-raw/md_summarize.rds")
street_names <- readRDS("data-raw/street_names.rds")
street_suffixes <- readRDS("data-raw/street_suffixes.rds")
unit_prefixes <- readRDS("data-raw/unit_prefixes.rds")

usethis::use_data(
  directions,
  directions_cardinal,
  directions_ordinal,
  ks_locations,
  md_summarize,
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



# Create ks_locations ####

## Import list copied from PDF to Excel

zips <- openxlsx::read.xlsx("data-raw/ZipCodes2018.xlsx")

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

openxlsx::write.xlsx(zips, "data-raw/ZipCodes2018_2.xlsx")


## Import list after manual cleaning

zips <- openxlsx::read.xlsx("data-raw/ZipCodes2018_2.xlsx")

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

saveRDS(ks_locations, "data-raw/ks_locations.rds")



# Modify ks_fips ####

ks_fips <- read.csv("data-raw/fips.csv")
colnames(ks_fips) <- tolower(colnames(ks_fips))
colnames(ks_fips)
colnames(ks_fips) <- c("fips", "st_fips", "cnty_fips", "cnty_code", "state", "county")
ks_fips$cnty_fips <- as.character(substr(ks_fips$fips, 3, 5))
ks_fips$fips <- as.character(ks_fips$fips)
ks_fips$st_fips <- as.character(ks_fips$st_fips)

saveRDS(ks_fips, "data-raw/ks_fips.rds")



