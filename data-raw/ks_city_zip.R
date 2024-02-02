# Generates `ks_city_zip.rda`

## source: https://www.ksrevenue.gov/5digitzip.html

library(tidyverse)

ks_city_zip <- readxl::read_xls("data-raw/5digitzip0424.xls")

ks_city_zip <- ks_city_zip %>%
  select(city = City, zip = "Zip Code", county = County) %>%
  mutate(across(everything(), str_squish))

# ks_city_zip <- ks_city_zip %>%
#   add_row(city = "Fort Dodge", zip = "67843", county = "Ford") %>%
#   arrange(city)

usethis::use_data(ks_city_zip, overwrite = T)

# Missing cities, zips, and combos are in dev-aux/helpers/: xcities.rds, xzips.rds, xks_city_zip.rds
