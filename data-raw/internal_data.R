# Generates internal data

load("data/address_regex.rda")
load("data/directions.rda")
load("data/epitrax_vars.rda")
load("data/ks_city_zip.rda")
load("data/ks_county_fips.rda")
load("data/ks_place_by_county.rda")
load("data/ks_places.rda")
load("data/melissa_data_codes.rda")
load("data/names.rda")
load("data/street_names.rda")
load("data/street_sfx.rda")
load("data/test_addresses.rda")
load("data/unit_pfx.rda")

usethis::use_data(
  address_regex,
  directions,
  epitrax_vars,
  ks_city_zip,
  ks_county_fips,
  ks_place_by_county,
  ks_places,
  melissa_data_codes,
  names,
  street_names,
  street_sfx,
  test_addresses,
  unit_pfx,
  internal = TRUE,
  overwrite = TRUE
)
