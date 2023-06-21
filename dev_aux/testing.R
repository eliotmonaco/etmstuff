


# id_distinct_rows ####

debugonce(id_distinct_rows)

df <- id_distinct_rows(
  df_addr_processed,
  var = c("street", "unit", "city", "state", "zip"),
  id_name = "address_registry_id",
  seq_start = 10000001,
  digits = 4
)



# classify_test_reason ####

debugonce(classify_test_reason)

df_test <- classify_test_reason(
  bl_data_2023q1_distinct,
  # bl_data_2023q1_distinct %>%
  #   filter(patient_id == "1351889"),
  # df2 = df_tr,
  bl_ref_val = 3.5,
  max_interval = 90
)

df_tr <- classify_test_reason(
  data,
  # df2 = df_tr,
  bl_ref_val = 3.5,
  max_interval = 90
)

df_tr %>%
  group_by(test_reason) %>%
  count()

df_check3 <- df_tr %>%
  filter(patient_id == "3286568") %>%
  arrange(patient_id, lab_collection_date, lab_specimen_source) %>%
  select(patient_id, lab_collection_date, lab_specimen_source,
         lab_result_symbol, lab_result_number, lab_name,
         lab_result_elev, bl_ref_val, test_reason,
         everything())

df_check4 <- df_tr %>%
  filter(patient_id == "2335363") %>%
  arrange(patient_id, lab_collection_date, lab_specimen_source) %>%
  select(patient_id, lab_collection_date, lab_specimen_source,
         lab_result_symbol, lab_result_number, lab_name,
         lab_result_elev, bl_ref_val, test_reason,
         everything())

# Same person/day/src

df_check <- df_tr %>%
  filter(test_reason == "CHECK")

df_check <- df_tr %>%
  filter(patient_id %in% df_check$patient_id) %>%
  arrange(patient_id, lab_collection_date, lab_specimen_source) %>%
  select(patient_id, lab_collection_date, lab_specimen_source,
         lab_result_symbol, lab_result_number, lab_name,
         lab_result_elev, bl_ref_val, test_reason,
         everything())

# Capillary test out of expected sequence

df_check2 <- df_tr %>%
  filter(test_reason == "unknown/other")

df_check2 <- df_tr %>%
  filter(patient_id %in% df_check2$patient_id) %>%
  arrange(patient_id, lab_collection_date, lab_specimen_source) %>%
  select(patient_id, lab_collection_date, lab_specimen_source,
         lab_result_symbol, lab_result_number, lab_name,
         lab_result_elev, bl_ref_val, test_reason,
         everything())



xl_sheets <- list(
  Same_src = df_check,
  Unexp_cap = df_check2
)

file_path <- "../test_rsn_recs.xlsx"

wb <- openxlsx::write.xlsx(
  xl_sheets,
  file_path,
  asTable = T,
  # tableStyle = "TableStyleMedium2",
  firstRow = T
)

# for (i in 1:length(xl_sheets)) {
#   openxlsx::setColWidths(
#     wb,
#     sheet = i,
#     cols = 1:ncol(xl_sheets[[i]]),
#     widths = "auto"#,
#     # hidden = c(
#     #   rep(T, times = 4),
#     #   rep(F, times = ncol(df) - 4)
#     # )
#   )
# }

openxlsx::saveWorkbook(wb, file_path, overwrite = T)


















# stoppage ####



# Removed from for loop, line 68. df_lookup was the dataframe filtered to create all_tests (where df1 %>% bind_rows(df2) is now)

# From df_lookup (df1 + df2)
df_lookup <- rbind(
  df1 %>%
    dplyr::select(
      row_id_src,
      patient_id,
      tidyselect::starts_with("lab"),
      test_reason_ks
    ),
  df2 %>%
    dplyr::select(
      row_id_src,
      patient_id,
      tidyselect::starts_with("lab"),
      test_reason_ks
    )
)



# format lab_collection_date with time ####

as.Date(epitrax_raw$lab_collection_date[100000],
        format = "%Y-%m-%d %H:%M:%S",
        origin = "1970-01-01")

as.POSIXct(epitrax_raw$lab_collection_date[100000],
           origin = "1970-01-01")

df <- epitrax_raw %>%
  filter(patient_id == "2335363")



# md_request functions ####

library(tidyverse)

df_md <- df_addr_processed %>%
  filter(sufficient_address) %>%
  select(
    md_id = address_id,
    street = street_final,
    unit:county
  )

df_md$md_url <- build_md_url(df_md, unit = "unit")

df_md$md_url <- str_replace(df_md$md_url, "https", "http")

x <- md_request(df_md$md_url[1])

df_test <- df_md[1:10,]
df_test$md_url[1] <- "garbage"
df_test$md_url[4] <- "garbage"
df_test$md_url[7] <- NA

dfA <- md_batch_request(df_test)

dfB <- send_md_request(df_test)
# Causes:
# Error in url(r[url], open = url_open_mode) :
# URL scheme unsupported by this method



# calculate_age ####

# Import EpiTrax source file
epitrax_raw <- readr::read_csv(
  unz(
    description = "dev_aux/test_data/2023-6-7 blood lead 2015 to 2023-3-31.zip",
    filename = "2023-6-7 blood lead 2015 to 2023-3-31.csv"
  ),
  col_types = readr::cols(.default = "c"),
  na = c("", "NA", "NULL")
)

epitrax_list <- config_epitrax(epitrax_raw)

epitrax_data <- epitrax_list[["data"]]
epitrax_keys <- epitrax_list[["keys"]]




epitrax_data$age <- calculate_age(
  d1 = epitrax_data$patient_birth_date,
  d2 = epitrax_data$lab_collection_date
)

epitrax_data <- epitrax_data %>%
  relocate(age, .after = patient_birth_date)



epitrax_data$age <- calculate_age(
  d1 = c("dog", "cat"),
  d2 = epitrax_data$lab_collection_date
)



# lab_results functions ####

df <- check_lab_results(epitrax_data, var = "lab_result_value")

epitrax_data_cln <- clean_lab_results(epitrax_data, var = "lab_result_value")

df <- check_lab_results(epitrax_data_cln, var = "lab_result_clean")

debugonce(parse_lab_results)
epitrax_data_cln <- parse_lab_results(epitrax_data_cln, var = "lab_result_clean")



df <- pull_addresses(epitrax_data, row_id = "src_row_id")



# Not sure ####

library(tidyverse)


# Sim data for Epitrax?


filename <- "dupesets_data"
path <- "helpers/"

xlpath <- paste0(gsub("/", "\\\\", getwd()), "\\", gsub("/", "\\\\", path), filename, ".xlsm")



df_addr <- df_addr_full %>%
  distinct(address_id, .keep_all = T)

df <- clean_values(
  df = df_addr,
  var = "street",
  id_var = "address_id",
  type = "pobox"
)

df_addr4 <- replace_values(
  df = df_addr,
  var = "street",
  id_var = "address_id",
  source = df
)

all.equal(df_addr2, df_addr4)

dfA <- validate_values(
  df = df,
  var = "city",
  type = "city"
)

dfA <- validate_values(
  df = df,
  var = "zip",
  type = "zip"
)

df <- simulate_data(rows = 1000, dirty = T)
