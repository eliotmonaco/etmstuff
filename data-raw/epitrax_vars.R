# Generates `epitrax_vars.rda`

# Read in variable names from data dictionary
epitrax_vars <- readxl::read_xlsx("C:/Users/eliot.monaco/OneDrive - State of Kansas, OITS/Documents/Lead/BL_Data_Dictionary_03_draft.xlsx")

epitrax_vars <- epitrax_vars[1:3]
colnames(epitrax_vars) <- c("group", "export_name", "epht_name")

epitrax_vars <- epitrax_vars |>
  dplyr::filter(group != "Removed" | is.na(group)) |>
  dplyr::mutate(
    epht_name = dplyr::if_else(is.na(epht_name), export_name, epht_name),
    final = dplyr::if_else(!is.na(group), TRUE, FALSE)
  )

usethis::use_data(epitrax_vars, overwrite = T)
