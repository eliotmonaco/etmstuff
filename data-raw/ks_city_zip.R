# Generates `ks_city_zip.rda`

## source: https://www.ksrevenue.gov/5digitzip.html

library(tidyverse)

ks_city_zip <- readxl::read_xls("data-raw/5digitzip0424.xls", col_types = "text")

ks_city_zip <- ks_city_zip %>%
  select(city = City, zip = "Zip Code", county = County) %>%
  mutate(across(everything(), str_squish))

ks_city_zip2 <- readxl::read_xlsx("data-raw/df_city_zip.xlsx", col_types = "text")

ks_city_zip <- ks_city_zip %>%
  mutate(across(everything(), str_squish)) %>%
  bind_rows(ks_city_zip2) %>%
  distinct() %>%
  arrange(city)

usethis::use_data(ks_city_zip, overwrite = T)
