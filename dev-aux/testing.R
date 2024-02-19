
library(tidyverse)
library(etmstuff)

devtools::load_all()




# clean_address ####

library(tidyverse)

df_addr_full <- readRDS("../bl_2023q2/data/addresses/df_addr_full.rds")

df_addr_full <- readRDS("~/r_projects/bl_2023q3/data/addresses/df_addr_full_2023q3.rds")

df_addr <- df_addr_full %>%
  distinct(address_id, .keep_all = T)

debugonce(clean_address)

df <- clean_address(df_addr, type = "embed_punct")

df_addr <- replace_values(df_addr, var = "street", df_src = df)



# Matches and non-matches
df_test <- data.frame(
  street = c(
    "123 Main St. #8",
    "456 9th St/PO Box 42",
    "456 9th St./PO Box 42",
    "999 1/2 Half Full Rd",
    "10,10 1/2 Grand Blvd Apt.5",
    "1010 1/.2 Grand Blvd Apt.5",
    "333 B.V.D Ave"
  )
)
df_test <- id_distinct_rows(df_test, "street", "address_id")

# Non-matches only
df_test2 <- data.frame(
  street = c(
    "123 Main St. #8",
    "999 1/2 Half Full Rd"
  )
)
df_test2 <- id_distinct_rows(df_test2, "street", "address_id")



debugonce(clean_address)

df <- clean_address(df_test, type = "embed_punct")



# No punctuation

df_addr_test <- df_addr

df_1a <- clean_address(df_addr_test, type = "num_dir1")

df_addr_test <- replace_values(df_addr_test, var = "street", df_src = df_1a)

df_1b <- clean_address(df_addr_test, type = "num_dir2")

df_addr_test <- replace_values(df_addr_test, var = "street", df_src = df_1b)

# With punctuation

df_addr_test <- df_addr

df_2a <- clean_address(df_addr_test, type = "num_dir1")

df_addr_test <- replace_values(df_addr_test, var = "street", df_src = df_2a)

df_2b <- clean_address(df_addr_test, type = "num_dir2")

df_addr_test <- replace_values(df_addr_test, var = "street", df_src = df_2b)

all.equal(df_1a, df_2a)
all.equal(df_1b, df_2b)



devtools::load_all()

df_test <- df_addr

df_test[1000, "street"] <- "w.w,w:w;w?w!w/w*w@w#w_w\"w[w]w{w}w(w)w"

df <- clean_address(df_test, type = "embed_punct")



devtools::load_all()

df_test <- df_addr

# df_orig <- clean_address(df_test, type = "unit")

df <- clean_address(df_test, type = "unit")

all.equal(df, df_orig)

df_diff1 <- df_orig %>%
  anti_join(df, by = "address_id")

df_diff2 <- df %>%
  anti_join(df_orig, by = "address_id")

df <- df %>%
  filter(address_id %in% c("078881", "027626", "070061", "100359", "021174", "046285", "021042", "051177"))



devtools::load_all()

df_test <- df_addr

df <- clean_address(df_test, type = "sep_unit")

df_diff1 <- df_orig %>%
  anti_join(df, by = "address_id")

df_diff2 <- df %>%
  anti_join(df_orig, by = "address_id")




all.equal(df, df_orig)






df_addr <- readRDS("../df_addr.rds")

devtools::load_all()

df2 <- etmstuff::clean_address(df_addr, type = "digit_letter")





street <- c(
  "500 NE 1st St.",
  "500 NE 1st",
  "500 1st St.",
  "500 1st",

  "500 NE 1s St.",
  "500 NE 1s",
  "500 1s St.",
  "500 1s",

  "500 NE 11th St.",
  "500 NE 11th",
  "500 11th St.",
  "500 11th",

  "500 NE 11s St.",
  "500 NE 11s",
  "500 11s St.",
  "500 11s"
)

df_test <- data.frame(street)

df_test$match <- stringr::str_match_all(df_test$street, p4)



df_addr <- readRDS("../df_addr.rds")

devtools::load_all()

df <- etmstuff::clean_address(df_addr, type = "six_words")





# stop ####



# stop ####





# md_results_table ####

df_md <- readRDS("../bl_2023q3/data/addresses/df_md_response1.rds")

md_results_table(df_md)



# pick_max_test_result ####

df <- pick_max_test_result(bl_data_2019, "all_tests")

all.equal(
  sort(unique(bl_data_2019$patient_id)),
  sort(unique(df$patient_id))
)



# merge_address_registry ####

rows <- sample(1:nrow(df_address_registry_2023q2), size = 1000, replace = F)

df_test_addr <- df_address_registry_2023q2[rows,]

df_test_addr <- df_test_addr %>%
  bind_rows(
    df_address_registry_2023q2 %>%
      slice(1:1000) %>%
      mutate(AddressLine1 = "1000 SW Jackson")
  ) %>%
  select(-address_registry_id) %>%
  arrange(AddressKey)

df_test_addr$dupe_id <- as.character(1:nrow(df_test_addr))

df_test_addr <- df_test_addr %>%
  relocate(dupe_id)

## original

# debugonce(merge_address_registry)

address_list1 <- merge_address_registry(df = df_test_addr, registry = df_address_registry_2023q2)

df_data_addr1 <- address_list1[["df_addr"]]
df_reg_addr1 <- address_list1[["df_registry"]]

## new - address

debugonce(cbls_registry_merge)

address_list2 <- cbls_registry_merge(
  df_data = df_test_addr,
  df_reg = df_address_registry_2023q2,
  type = "address"
)

df_data_addr2 <- address_list2[["df_data"]]
df_reg_addr2 <- address_list2[["df_registry"]]

all.equal(df_data_addr1, df_data_addr2 %>% relocate(address_registry_id, .after = dupe_id))
all.equal(df_reg_addr1, df_reg_addr2)

## new - child

df_test_child <- data_cbls_2023q2 %>%
  select(-child_registry_id)

debugonce(cbls_registry_merge)

child_list <- cbls_registry_merge(
  df_data = df_test_child,
  df_reg = df_child_registry_2023q1,
  type = "child"
)

df_data_child <- child_list[["df_data"]]
df_reg_child <- child_list[["df_registry"]]

all.equal(df_data_child, data_cbls_2023q2)



# my_xl_table, my_xlsx ####

my_xl_table(mtcars, "cars_wkbk", c("mpg", "cyl"))

my_xl_table(mtcars, "cars_wkbk", as_table = F)



# my_xlsx: works with or wo ".xlsx" in name? how to name separate sheets? (use ...?)



openxlsx::write.xlsx(mtcars, "cars")



df <- data.frame()

xl_sheets <- list(
  dfA = data.frame(),
  dfB = data.frame(),
  dfC = mtcars
)

debugonce(my_xlsx)

my_xlsx(mtcars, "cars", sheetName = "bigfastcars")



# pick_max_cfm_test ####

data <- subset_date_range(data_core_2015_2022, "lab_collection_date", "2022")

data %>%
  summarize(max_age = max(age))

data %>%
  group_by(test_reason) %>%
  count()

data %>%
  group_by(patient_id) %>%
  summarize(sum(test_reason == "cap_scrn"))



debugonce(pick_max_cfm_test)

df <- pick_max_cfm_test(data)

length(unique(data$patient_id))



# usps_lookup ####

df <- data.frame(
  id = c("KDHE BEPHI", "Curtis State Office Bldg"),
  street = c("1000 SW Jackson", "1000 SW Jackson"),
  unit = c("Suite 110", NA),
  city = c("Topeka", "Topeka"),
  state = c("KS", "KS"),
  zip = c("66612", "66612")
)

df$c1 <- NA
df$c2 <- NA

debugonce(usps_lookup)

df_results <- usps_lookup(df, row_id = "id")

df_results <- usps_lookup(df)



# sim_address ####

debugonce(sim_address)
df <- sim_address(5000)

debugonce(sim_unit)
v <- sim_unit(100)



# fuzzy_compare ####

## flex_compare instead??

df1 <- sim_address(nrow = 10)
df2 <- sim_address(nrow = 5000)

debugonce(fuzzy_compare)

df_match <- fuzzy_compare(
  df1,
  df2,
  fuzzy_var = c("street", "unit"),
  exact_var = c("city", "state", "zip")
)



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
