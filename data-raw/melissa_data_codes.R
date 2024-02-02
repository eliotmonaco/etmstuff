# Generates `melissa_data_codes.rda`

## source: https://wiki.melissadata.com/index.php?title=Result_Code_Details#Personator_Consumer

## Copied from web page on 2024-02-01

library(tidyverse)

df_md <- readxl::read_xlsx("data-raw/melissa_data_info_20240201.xlsx", sheet = "Sheet1")

colnames(df_md) <- tolower(colnames(df_md))

melissa_data_codes <- list()

melissa_data_codes$address_status <- df_md %>%
  select(-category) %>%
  filter(str_detect(code, "^AS"))

melissa_data_codes$address_error <- df_md %>%
  select(-category) %>%
  filter(str_detect(code, "^AE"))

melissa_data_codes$address_change <- df_md %>%
  select(-category) %>%
  filter(str_detect(code, "^AC"))

melissa_data_codes$geocode_status <- df_md %>%
  select(-category) %>%
  filter(str_detect(code, "^GS"))

melissa_data_codes$geocode_error <- df_md %>%
  select(-category) %>%
  filter(str_detect(code, "^GE"))

usethis::use_data(melissa_data_codes, overwrite = T)
