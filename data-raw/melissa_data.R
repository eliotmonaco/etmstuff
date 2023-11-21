# Generates `melissa_data_result_codes`



library(tidyverse)

df_md <- openxlsx::read.xlsx("data-raw/melissa_data_info.xlsx", sheet = "Sheet1")

colnames(df_md) <- tolower(colnames(df_md))

melissa_data_result_codes <- list()

melissa_data_result_codes[["address_status"]] <- df_md %>%
  select(-category) %>%
  filter(str_detect(code, "^AS"))

melissa_data_result_codes[["address_error"]] <- df_md %>%
  select(-category) %>%
  filter(str_detect(code, "^AE"))

melissa_data_result_codes[["address_change"]] <- df_md %>%
  select(-category) %>%
  filter(str_detect(code, "^AC"))

melissa_data_result_codes[["geocode_status"]] <- df_md %>%
  select(-category) %>%
  filter(str_detect(code, "^GS"))

melissa_data_result_codes[["geocode_error"]] <- df_md %>%
  select(-category) %>%
  filter(str_detect(code, "^GE"))

usethis::use_data(melissa_data_result_codes, overwrite = T)
