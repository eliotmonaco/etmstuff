# Generates `ks_place_by_county.rda`

## source: https://www.census.gov/library/reference/code-lists/ansi.html#place

ks_place_by_county <- read.table(
  file = "data-raw/ks_2020_place_by_county.txt",
  header = T,
  sep = "|",
  colClasses = "character"
)

usethis::use_data(ks_place_by_county, overwrite = T)
