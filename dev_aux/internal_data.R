# Generate internal data for package

directions_cardinal <- readRDS("dev_aux/helpers/directions_cardinal.rds")
directions_ordinal <- readRDS("dev_aux/helpers/directions_ordinal.rds")
epitrax_variables <- readRDS("dev_aux/helpers/epitrax_variables.rds")
postmastr_directions <- readRDS("dev_aux/helpers/postmastr_directions.rds")
ks_cities <- readRDS("dev_aux/helpers/ks_cities.rds")
ks_zipcodes <- readRDS("dev_aux/helpers/ks_zipcodes.rds")
regex_pobox <- readRDS("dev_aux/helpers/regex_pobox.rds")
regex_various <- readRDS("dev_aux/helpers/regex_various.rds")
street_suffix <- readRDS("dev_aux/helpers/street_suffix.rds")
directions <- readRDS("dev_aux/helpers/directions.rds")
common_streets <- readRDS("dev_aux/helpers/common_streets.rds")
common_street_suffixes <- readRDS("dev_aux/helpers/common_street_suffixes.rds")
common_unit_prefixes <- readRDS("dev_aux/helpers/common_unit_prefixes.rds")

usethis::use_data(
  directions_cardinal,
  directions_ordinal,
  epitrax_variables,
  postmastr_directions,
  ks_cities,
  ks_zipcodes,
  regex_pobox,
  regex_various,
  directions,
  common_streets,
  common_street_suffixes,
  common_unit_prefixes,
  internal = T, overwrite = T
)






