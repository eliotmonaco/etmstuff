# Generates `ks_places.rda`

## source: https://www.census.gov/library/reference/code-lists/ansi.html#place

ks_places <- read.table(
  file = "data-raw/ks_2020_place_table.txt",
  header = T,
  sep = "|",
  colClasses = "character"
)

usethis::use_data(ks_places, overwrite = T)
