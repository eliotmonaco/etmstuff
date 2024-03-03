# Generates `melissa_data_codes.rda`

## source: https://wiki.melissadata.com/index.php?title=Result_Code_Details#Personator_Consumer
## Copied from web page on 2024-02-01

df_md <- readxl::read_xlsx("data-raw/melissa_data_info_20240201.xlsx", sheet = "Sheet1")

colnames(df_md) <- tolower(colnames(df_md))

melissa_data_codes <- list()

p <- c("^AS", "^AE", "^AC", "^GS", "^GE")
name <- c("address_status", "address_error", "address_change", "geocode_status", "geocode_error")

for (i in 1:length(p)) {
  melissa_data_codes[[name[i]]] <- df_md |>
    dplyr::select(-category) |>
    dplyr::filter(stringr::str_detect(code, p[i]))
}

usethis::use_data(melissa_data_codes, overwrite = T)
