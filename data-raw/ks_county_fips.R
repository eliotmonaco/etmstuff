# Generates `ks_county_fips.rda`

## source: https://www.census.gov/library/reference/code-lists/ansi.html#cou

ks_county_fips <- read.table(
  file = "data-raw/ks_2020_counties.txt",
  header = T,
  sep = "|",
  colClasses = "character"
)

ks_county_fips <- ks_county_fips |>
  tibble::as_tibble() |>
  dplyr::mutate(county = stringr::str_squish(stringr::str_remove(COUNTYNAME, " County"))) |>
  dplyr::select(
    county, county_full = COUNTYNAME, state_fips = STATEFP, county_fips = COUNTYFP,
    county_ns = COUNTYNS, fips_class = CLASSFP, func_stat = FUNCSTAT
  )

usethis::use_data(ks_county_fips, overwrite = T)
