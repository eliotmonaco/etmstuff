# Generates `ks_city_zip.rda`

## source: https://www.ksrevenue.gov/5digitzip.html

ks_city_zip <- readxl::read_xls("data-raw/5digitzip0424.xls", col_types = "text")

ks_city_zip <- ks_city_zip |>
  dplyr::select(city = City, zip = "Zip Code", county = County) |>
  dplyr::mutate(dplyr::across(tidyselect::everything(), stringr::str_squish))

ks_city_zip2 <- readxl::read_xlsx("data-raw/df_city_zip.xlsx", col_types = "text")

ks_city_zip <- ks_city_zip |>
  dplyr::mutate(dplyr::across(tidyselect::everything(), stringr::str_squish)) |>
  dplyr::bind_rows(ks_city_zip2) |>
  dplyr::distinct() |>
  dplyr::arrange(city)

usethis::use_data(ks_city_zip, overwrite = T)
