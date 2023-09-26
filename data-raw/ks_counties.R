## `ks_counties` dataset

### source: https://www.census.gov/library/reference/code-lists/ansi.html#cou

ks_counties <- read.table(
  file = "data-raw/ks_2020_counties.txt",
  header = T,
  sep = "|",
  colClasses = "character"
)

usethis::use_data(ks_counties, overwrite = T)
