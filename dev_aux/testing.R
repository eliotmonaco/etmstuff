# classify_test_reason ####

debugonce(classify_test_reason)
df <- classify_test_reason(
  bl_data_2023q1_distinct,
  bl_ref_val = 3.5,
  max_interval = 92
)



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
