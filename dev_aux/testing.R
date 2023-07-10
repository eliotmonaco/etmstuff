
library(tidyverse)
library(etmstuff)







# stop ####




# cbls_investigation_table ####

key <- cbls_table_key(data_cbls_2023q1, action = "add")

debugonce(cbls_investigation_table)

df_tbl_inv <- cbls_investigation_table(
  df_inv,
  key = key
)

debugonce(cbls_check_table)

df <- cbls_check_table(df_tbl_inv)



# cbls_child_table ####

key <- cbls_table_key(data_cbls_2023q1, action = "add")

debugonce(cbls_child_table)

df_tbl_chi <- cbls_child_table(
  data_cbls_2023q1,
  key = key,
  row_id = "dupe_id"
)

debugonce(cbls_check_table)

df <- cbls_check_table(df_tbl_chi)



# cbls_address_table ####

key <- cbls_table_key(data_cbls_2023q1, action = "add")

debugonce(cbls_address_table)

df_tbl_add <- cbls_address_table(
  data_cbls_2023q1,
  key = key,
  registry = df_address_registry_2023q1
)

debugonce(cbls_check_table)

df <- cbls_check_table(df_tbl_add)



# cbls_lab_table ####

key <- cbls_table_key(data_cbls_2023q1, action = "add")

df_tbl_lab <- cbls_lab_table(
  data_cbls_2023q1,
  row_id = "dupe_id",
  key = key,
  ref_lab_type = df_lab_type,
  ref_scrn_site = df_scrn_site
)

debugonce(cbls_check_table)

df <- cbls_check_table(df_tbl_lab)



# cbls_undupe ####

debugonce(cbls_undupe)

data_cbls_new_undupe <- cbls_undupe(data_cbls_preundupe, row_id = "dupe_id")

all.equal(
  data_cbls_new_undupe %>%
    arrange(dupe_id),
  data_cbls_old_undupe %>%
    arrange(dupe_id)
)

ids <- data_cbls_new_undupe %>%
  select(dupe_id, cbls_duplicate) %>%
  left_join(
    data_cbls_old_undupe %>%
      select(dupe_id, cbls_dupe),
    by = "dupe_id"
  ) %>%
  filter(cbls_duplicate != cbls_dupe) %>%
  pull(dupe_id)

dfA <- data_cbls_old_undupe %>%
  filter(dupe_id %in% ids) %>%
  arrange(dupe_id) %>%
  relocate(cbls_dupe)

dfB <- data_cbls_new_undupe %>%
  filter(dupe_id %in% ids) %>%
  arrange(dupe_id) %>%
  relocate(cbls_duplicate)

data_cbls_old_undupe %>%
  group_by(cbls_dupe) %>%
  count()

data_cbls_new_undupe %>%
  group_by(cbls_duplicate) %>%
  count()



# merge_child_registry ####

data_cbls <- readRDS("../bl_2023q1/data/cbls/data_cbls_2023q1.rds")
df_child_registry <- readRDS("../bl_2023q1/data/registries/df_child_registry.rds")

debugonce(merge_child_registry)

child_list <- merge_child_registry(data_cbls, registry = df_child_registry)

df_data <- child_list[["df_data"]]
df_registry <- child_list[["df_registry"]]



# merge_address_registry ####

df_addr_cbls <- readRDS("../bl_2023q1/data/addresses/df_addr_cbls.rds")
df_address_registry <- readRDS("../bl_2023q1/data/registries/df_address_registry.rds")

debugonce(merge_address_registry)

addr_list <- merge_address_registry(df_addr_cbls, registry = df_address_registry)

df_addr <- addr_list[["df_addr"]]
df_registry <- addr_list[["df_registry"]]



# cbls_table_key ####

data %>%
  dplyr::group_by(blood_lead_poisoning_form_col_bl_funding_source) %>%
  dplyr::count() %>%
  print(n = nrow(.))

data_2023q1 <- subset_date_range(data, var = "lab_collection_date", range = "2023q1")

key <- cbls_table_key(data_2023q1, action = "add")



# cbls_check_table ####

debugonce(cbls_check_table)

df <- cbls_check_table(cbls_tbls_list[["df_2018q3_add"]])



# undupe ####

library(etmstuff)

data <- subset_date_range(
  epitrax_data,
  var = "lab_collection_date",
  range = "2023q1"
)

# Current version
undp1 <- undupe(
  data,
  visible_var = c(
    "patient_id",
    "lab_collection_date",
    "lab_result_value",
    "lab_specimen_source"
  )
)

# Test version
undp2 <- undupe(
  data,
  visible_var = c(
    "patient_id",
    "lab_collection_date",
    "lab_result_value",
    "lab_specimen_source"
  )
)

all.equal(undp1, undp2)



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

library(tidyverse)
library(etmstuff)

data_core_tr_2015 <- subset_date_range(
  data_core,
  var = "lab_collection_date",
  range = "2015"
)

debugonce(classify_test_reason)

data_core_tr_2015 <- classify_test_reason(
  data_core_tr_2015,
  bl_ref_val = 5,
  max_interval = 90
)



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
