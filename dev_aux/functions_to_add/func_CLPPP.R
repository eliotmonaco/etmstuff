#### address_registry_lookup() ####

address_registry_lookup <- function(df, registry) {
  
  source("2_functions/func_general.R")
  
  var_check(df, var = c("NoUnit_Flag", "CLPPP_Flag"))
  var_check(registry, var = c("pbAddrID", "CDC_Flag"))
  
  suppressWarnings({
    library(dplyr)
  })
  
  # IDs assigned to soft-deleted addresses
  skip_ids <- registry %>%
    filter(Deleted) %>%
    select(pbAddrID)
  
  # Create lookup value in geocoded DF
  df <- standardize(df, replace = NA, uppercase = F)
  vars_df_unit <- c("AddressHouseNumber", "AddressPreDirection", "AddressStreetName", "AddressStreetSuffix",
                    "AddressPostDirection", "AddressSuiteName", "AddressSuiteNumber", "City", "CountyName",
                    "State", "NoUnit_Flag")
  vars_df_nounit <- c("AddressHouseNumber", "AddressPreDirection", "AddressStreetName", "AddressStreetSuffix",
                      "AddressPostDirection", "City", "CountyName", "State", "NoUnit_Flag")
  df <- df %>%
    filter(NoUnit_Flag == F) %>% # Include unit in lookup
    tidyr::unite(lookup, all_of(vars_df_unit), sep = "", remove = F, na.rm = T) %>%
    full_join(df %>%
                filter(NoUnit_Flag == T) %>% # Exclude unit from lookup
                tidyr::unite(lookup, all_of(vars_df_nounit), sep = "", remove = F, na.rm = T),
              by = c(colnames(df), "lookup")) %>%
    relocate(lookup)
  df$lookup <- stringr::str_to_upper(stringr::str_remove_all(df$lookup, "\\s"))
  
  # Create lookup value in registry
  registry <- standardize(registry %>% filter(!Deleted | is.na(Deleted)), replace = NA, uppercase = F)
  vars_reg_suite <- c("StNum", "DirPrefix", "StName", "StSuffix", "DirSuffix", "SuiteName", "SuiteNum",
                      "City", "County", "State", "NoUnit_Flag")
  vars_reg_nosuite <- c("StNum", "DirPrefix", "StName", "StSuffix", "DirSuffix",
                        "City", "County", "State", "NoUnit_Flag")
  registry <- registry %>%
    filter(NoUnit_Flag == "FALSE") %>% # Include unit in lookup
    tidyr::unite(lookup, all_of(vars_reg_suite), sep = "", remove = F, na.rm = T) %>%
    full_join(registry %>%
                filter(NoUnit_Flag == "TRUE") %>% # Exclude unit from lookup
                tidyr::unite(lookup, all_of(vars_reg_nosuite), sep = "", remove = F, na.rm = T),
              by = c(colnames(registry), "lookup")) %>%
    relocate(lookup)
  registry$lookup <- stringr::str_to_upper(stringr::str_remove_all(registry$lookup, "\\s"))
  
  # Count of total & unique addresses in registry (by lookup value)
  n <- length(registry$lookup[which(registry$lookup != "")])
  n_unq <- length(unique(registry$lookup[which(registry$lookup != "")]))
  
  # Get full list of pbAddrID before deduplication of registry
  pbAddrID_all <- registry$pbAddrID
  
  # Stop if duplicates exist (currently circumvented)
  if (n > n_unq) {
    # stop(message())
    message("There are duplicate addresses in the registry.")
    registry <- registry %>%
      distinct(lookup, .keep_all = T)
  }
  
  # If the lookup value from the geocoded DF is found in the registry, add the pbAddrID from the registry to the DF
  f <- function(x) {
    if (x %in% registry$lookup) {
      registry$pbAddrID[which(registry$lookup == x)]
    } else {
      NA
    }
  }
  
  df$pbAddrID <- as.character(do.call(rbind, lapply(df$lookup, f)))
  
  # Get highest ID assigned to addresses with a MAK
  id_max <- max(as.numeric(str_remove_all(pbAddrID_all, "^9")))
  
  # Addresses already in registry
  df1 <- df %>%
    filter(!is.na(pbAddrID)) %>%
    mutate(addr_reg_status = "exists")
  
  # New validated addresses (with MAK)
  df2 <- df %>% # Deduplicate by lookup value so that addresses flagged NoUnit_Flag == T receive the same pbAddrID
    filter(is.na(pbAddrID), ValidAddr_Flag == T, !is.na(MelissaAddressKey))
  df2 <- assign_seq_id(df2, id = "id1", id_vars = "lookup", key = "address_id")
  df2_pbAddrID <- df2 %>%
    distinct(id1, .keep_all = T) %>%
    mutate(pbAddrID = skipper_seq(id_max + 1, nrow(.), skip_ids), addr_reg_status = "new")
  df2 <- df2 %>%
    select(address_id, id1) %>%
    left_join(df2_pbAddrID %>%
                select(-address_id),
              by = "id1") %>%
    select(-id1)
  
  # Get highest ID assigned to addresses without a MAK
  id_max <- max(as.numeric(str_remove_all(pbAddrID_all, "^[^9]")))
  
  # New validated addresses (without MAK)
  df3 <- df %>% # Deduplicate by lookup value so that addresses flagged NoUnit_Flag == T receive the same pbAddrID
    filter(is.na(pbAddrID), ValidAddr_Flag == T, is.na(MelissaAddressKey))
  df3 <- assign_seq_id(df3, id = "id1", id_vars = "lookup", key = "address_id")
  df3_pbAddrID <- df3 %>%
    distinct(id1, .keep_all = T) %>%
    mutate(pbAddrID = skipper_seq(id_max + 1, nrow(.), skip_ids), addr_reg_status = "new_no_MAK")
  df3 <- df3 %>%
    select(address_id, id1) %>%
    left_join(df3_pbAddrID %>%
                select(-address_id),
              by = "id1") %>%
    select(-id1)
  
  # Remaining (not validated) addresses
  df4 <- df %>%
    filter(is.na(pbAddrID), ValidAddr_Flag == F) %>%
    mutate(addr_reg_status = "not_validated")
  
  rbind(df1, df2, df3, df4) %>%
    relocate(pbAddrID, addr_reg_status)
  
}



#### address_registry_add() ####

address_registry_add <- function(df, registry) {
  
  source("2_functions/func_general.R")
  source("2_functions/func_data_prep.R")
  
  var_check(df, var = c("pbAddrID", "addr_reg_status"))
  var_check(registry, var = c("pbAddrID", "CDC_Flag"))
  
  suppressWarnings({
    library(dplyr)
  })
  
  vars_df <- c("pbAddrID", "MelissaAddressKey", "MelissaAddressKeyBase",
               "NoUnit_Flag", "PCM_Flag", "EWS_Flag", "NoUSPS_Flag", "Bus_Flag", "ValidAddr_Flag",
               "CLPPP_Flag", "DeliveryIndicator", "street_geo", "CityAbbreviation", "PostalCode", "State",
               "AddressHouseNumber", "AddressPreDirection", "AddressStreetName", "AddressStreetSuffix", "AddressPostDirection",
               "AddressSuiteName", "AddressSuiteNumber", "AddressPrivateMailboxName", "AddressPrivateMailboxRange",
               "CountyName", "CountyFIPS", "CensusBlock", "CensusTract", "GeoType", "Latitude", "Longitude")
  df <- df %>%
    filter(stringr::str_detect(addr_reg_status, "new")) %>%
    select(addr_reg_status, all_of(vars_df)) %>%
    distinct(pbAddrID, .keep_all = T) %>%
    rename(MAK = MelissaAddressKey, BaseMAK = MelissaAddressKeyBase, CDC_Flag = CLPPP_Flag, AddrType = DeliveryIndicator,
           ZIP4 = PostalCode, StNum = AddressHouseNumber, DirPrefix = AddressPreDirection, DirSuffix = AddressPostDirection,
           PrvtMailBxName = AddressPrivateMailboxName, PrvtMailBxNum = AddressPrivateMailboxRange) %>%
    mutate(AddrID_cat = case_when(is.na(MAK) & is.na(BaseMAK) ~ "no_mak",
                                  T ~ "standard"),
           Addr1 = toupper(street_geo),
           Addr2 = NA,
           City = toupper(CityAbbreviation),
           StName = toupper(AddressStreetName),
           StSuffix = toupper(AddressStreetSuffix),
           SuiteName = toupper(AddressSuiteName),
           SuiteNum = toupper(AddressSuiteNumber),
           County = toupper(CountyName),
           ZIP = substr(ZIP4, 1, 5),
           ZIP_CDC = sub("-", "", ZIP4),
           Latitude = as.numeric(Latitude),
           Longitude = as.numeric(Longitude),
           DateAdded = Sys.Date(),
           Deleted = F) %>%
    select(pbAddrID, AddrID_cat, MAK, BaseMAK, NoUnit_Flag, PCM_Flag, EWS_Flag, NoUSPS_Flag, Bus_Flag,
           ValidAddr_Flag, CDC_Flag, AddrType, Addr1, Addr2, City, ZIP, ZIP4, ZIP_CDC,
           State, StNum, DirPrefix, StName, StSuffix, DirSuffix, SuiteName, SuiteNum, PrvtMailBxName,
           PrvtMailBxNum, County, CountyFIPS, CensusBlock, CensusTract, GeoType, Latitude, Longitude, DateAdded,
           Deleted)
  
  registry %>%
    full_join(df, by = colnames(df))
  
}



#### child_registry_lookup() ####

child_registry_lookup <- function(df, registry) {
  
  source("2_functions/func_general.R")
  
  var_check(df, var = c("recno", "hash_value", "patient_record_number", "age"))
  var_check(registry, var = c("child_id", "cmr"))
  
  if (!all(df$age < 6)) stop("The dataframe should contain only records for children under 6 years old", call. = F)
  
  suppressWarnings({
    library(dplyr)
  })
  
  # IDs assigned to soft-deleted addresses
  skip_ids <- registry %>%
    filter(deleted) %>%
    select(child_id)
  
  # Count of total & unique IDs in registry
  n_id <- length(registry$child_id[which(str_detect(registry$child_id, "\\d+"))])
  n_id_unq <- length(unique(registry$child_id[which(str_detect(registry$child_id, "\\d+"))]))
  
  # Count of total & unique CMRs in registry
  n_cmr <- length(registry$cmr[which(str_detect(registry$cmr, "\\d+"))])
  n_cmr_unq <- length(unique(registry$cmr[which(str_detect(registry$cmr, "\\d+"))]))
  
  # Stop if duplicates exist (currently circumvented)
  if (n_id > n_id_unq | n_cmr > n_cmr_unq) {
    # stop(message())
    message("There are duplicate IDs or CMRs in the registry.")
    registry <- registry %>%
      filter(!deleted | is.na(deleted)) %>%
      distinct(cmr, .keep_all = T)
  }
  
  # Subset unique CMRs in df
  df_cmrs <- df %>%
    select(patient_record_number) %>%
    distinct()
  
  # If the CMR from df is found in the registry, add the child_id from the registry to df
  f <- function(x) {
    if (x %in% registry$cmr) {
      registry$child_id[which(registry$cmr == x)]
    } else {
      NA
    }
  }
  
  df_cmrs$child_id <- as.character(do.call(rbind, lapply(df_cmrs$patient_record_number, f)))
  
  # Get highest ID assigned
  id_max <- max(as.numeric(registry$child_id))
  
  # Children already in registry
  df_cmrs1 <- df_cmrs %>%
    filter(!is.na(child_id)) %>%
    mutate(child_reg_status = "exists")
  
  # New children
  df_cmrs2 <- df_cmrs %>%
    filter(is.na(child_id)) %>%
    mutate(child_id = skipper_seq(id_max + 1, nrow(.), skip_ids), child_reg_status = "new")
  
  df_cmrs <- rbind(df_cmrs1, df_cmrs2)
  
  df %>%
    left_join(df_cmrs, by = "patient_record_number")
  
}



#### child_registry_add() ####

child_registry_add <- function(df, registry) {
  
  source("2_functions/func_general.R")
  
  var_check(df, var = c("child_id", "child_reg_status", "patient_record_number", "lab_collection_date"))
  var_check(registry, var = c("child_id", "cmr"))
  
  y <- get_yr(df, var = "lab_collection_date")
  q <- get_qtr(df, var = "lab_collection_date")
  
  if (length(y) != 1 | length(q) != 1) {
    stop("The dataframe should contain only records for a single year and quarter", call. = F)
  }
  
  suppressWarnings({
    library(dplyr)
  })
  
  vars_df <- c("child_id", "patient_record_number")
  
  df <- df %>%
    filter(stringr::str_detect(child_reg_status, "new")) %>%
    select(all_of(vars_df)) %>%
    distinct(child_id, .keep_all = T) %>%
    rename(cmr = patient_record_number) %>%
    mutate(qtr = paste0(y, "q", q),
           deleted = F,
           del_date = as.Date.numeric(NA_real_, format = "%Y-%m-%d", origin = "1970-01-01"),
           reason = NA_character_,
           user = Sys.info()[["user"]])
  
  if (nrow(df) == 0) {
    return(message("No new children to add to the child registry"))
  }
  
  rbind(registry, df)
  
}



#### select_clppp_address() ####

select_clppp_address <- function(df, addresses) {
  
  source("2_functions/func_general.R")
  
  var_check(df, var = c("recno", "hash_value", "patient_record_number", "age"))
  var_check(addresses, var = c("recno", "address_src", "address_id", "pbAddrID"))
  
  if (!all(df$age < 6)) stop("The dataframe should contain only records for children under 6 years old", call. = F)
  
  suppressWarnings({
    library(stringr)
    library(dplyr)
  })
  
  df1 <- df %>%
    left_join(addresses %>%
                filter(address_src == "1_lab", CLPPP_Flag) %>%
                select(recno, pbAddrID, address_src),
              by = "recno")
  
  df2 <- df1 %>%
    filter(is.na(pbAddrID)) %>%
    select(-pbAddrID, -address_src) %>%
    left_join(addresses %>%
                filter(address_src == "2_cur", CLPPP_Flag) %>%
                select(recno, pbAddrID, address_src),
              by = "recno")
  
  df3 <- df2 %>%
    filter(is.na(pbAddrID)) %>%
    select(-pbAddrID, -address_src) %>%
    left_join(addresses %>%
                filter(address_src == "3_man", CLPPP_Flag) %>%
                select(recno, pbAddrID, address_src),
              by = "recno")
  
  df4 <- rbind(df1 %>% filter(!is.na(pbAddrID)),
               df2 %>% filter(!is.na(pbAddrID)),
               df3)
  
  if (!identical(sort(df$recno), sort(df4$recno))) stop("Output dataframe doesn't match input dataframe", call. = F)
  
  df4
  
}



#### clppp_undupe_lab_tests() ####

# Unduplicates the formatted lab table, keeping only one record per child
# per sample date according to the CDC submission guidelines. The CDC
# guidelines do not indicate how to evaluate records with an unknown sample
# type (SAMP_TYPE == 9). Currently they are ignored, and a message is generated
# when all records in a dupeset have an unknown sample type.

clppp_undupe_lab_tests <- function(df) {
  
  source("2_functions/func_general.R")
  source("2_functions/func_undupe.R")
  
  var_check(df, var = c("recno", "hash_value", "patient_record_number", "age",
                        "lab_collection_date", "lab_specimen_source", "lab_result_number"))
  
  if ("dupe_id_lab" %in% colnames(df)) stop("The dataframe already contains a variable named `dupe_id_lab`. Rename this variable to proceed.", call. = F)
  
  if (!all(df$age < 6)) stop("The dataframe should contain only records for children under 6 years old", call. = F)
  
  suppressWarnings({
    library(dplyr)
  })
  
  # Find duplicates based on CMR and sample date
  df_dupesets <- undupe(df, undupe_vars = c("patient_record_number", "lab_collection_date"),
                        id_name = "dupe_id_lab")[["df_dupesets"]]
  
  dupe_ids <- data.frame(id = unique(df_dupesets$dupe_id_lab))
  
  # DF to hold retained duplicate tests
  df_keep <- df[0,]
  
  # Algorithm to resolve duplicate tests
  for (i in 1:nrow(dupe_ids)) {
    id <- dupe_ids$id[i]
    df2 <- df_dupesets %>%
      filter(dupe_id_lab == id)
    # if (any(df2$SAMP_TYPE == 1)) {
    if (any(stringr::str_detect(df2$lab_specimen_source, regex("venous", ignore_case = T)))) {
      # If samples are all venous, take the highest test result.
      # If samples are mixed capillary and venous, take the (highest) venous.
      df2 <- df2 %>%
        filter(stringr::str_detect(df2$lab_specimen_source, regex("venous", ignore_case = T))) %>%
        filter(lab_result_number == max(lab_result_number))
      df_keep <- rbind(df_keep, df2[1,])
    } else if (any(stringr::str_detect(df2$lab_specimen_source, regex("capillary", ignore_case = T)))) {
      # If the samples are all capillary, take the lowest test result.
      df2 <- df2 %>%
        filter(stringr::str_detect(df2$lab_specimen_source, regex("capillary", ignore_case = T))) %>%
        filter(lab_result_number == min(lab_result_number))
      df_keep <- rbind(df_keep, df2[1,])
    } else {
      message("Unknown sample types in df$recno ", paste(df2$recno, collapse = " and "))
    }
  }
  
  df_keep <- df_keep %>%
    select(-c(dupe_id_lab, dupe_type)) %>%
    mutate(clppp_duplicate = "true_keep")
  
  df_remove <- df_dupesets %>%
    anti_join(df_keep, by = "recno") %>%
    select(-c(dupe_id_lab, dupe_type)) %>%
    mutate(clppp_duplicate = "true_remove")
  
  df_nondupes <- df %>%
    anti_join(df_keep, by = "recno") %>%
    anti_join(df_remove, by = "recno") %>%
    mutate(clppp_duplicate = "false")
  
  rbind(df_keep, df_remove, df_nondupes) %>%
    arrange(lab_collection_date, patient_record_number, clppp_duplicate)
  
}



#### classify_test_reason() ####

# df1 = data_core for current quarter
# df2 = previous data_core containing test_reason_ks variable

classify_test_reason <- function(df1, df2=NULL, ref_val) {
  
  source("2_functions/func_general.R")
  
  var_check(df1, var = c("recno", "hash_value", "patient_record_number",
                         "lab_result_number", "lab_result_symbol"))
  
  if (!is.null(df2)) {
    var_check(df2, var = c("recno", "hash_value", "patient_record_number",
                           "lab_result_number", "lab_result_symbol", "test_reason_ks"))
  }
  
  suppressWarnings({
    library(dplyr)
    library(stringr)
  })
  
  var_nm <- paste0("ref_val_", Sys.Date())
  
  # Add test_reason_ks (empty) & lab_result_elev (T/F)
  df1 <- df1 %>%
    arrange(lab_collection_date, patient_record_number) %>%
    mutate(lab_result_elev = case_when(is.na(lab_result_symbol) & lab_result_number < ref_val ~ F,
                                       is.na(lab_result_symbol) & lab_result_number >= ref_val ~ T,
                                       lab_result_symbol == "<" & lab_result_number <= ref_val ~ F,
                                       lab_result_symbol == "<" & lab_result_number > ref_val ~ NA,
                                       lab_result_symbol == ">" & lab_result_number < ref_val ~ NA,
                                       lab_result_symbol == ">" & lab_result_number >= ref_val ~ T),
           {{ var_nm }} := ref_val,
           test_reason_ks = NA)
  
  if (!is.null(df2)) {
    df2 <- df2 %>%
      arrange(lab_collection_date, patient_record_number) %>%
      mutate(lab_result_elev = case_when(is.na(lab_result_symbol) & lab_result_number < ref_val ~ F,
                                         is.na(lab_result_symbol) & lab_result_number >= ref_val ~ T,
                                         lab_result_symbol == "<" & lab_result_number <= ref_val ~ F,
                                         lab_result_symbol == "<" & lab_result_number > ref_val ~ NA,
                                         lab_result_symbol == ">" & lab_result_number < ref_val ~ NA,
                                         lab_result_symbol == ">" & lab_result_number >= ref_val ~ T),
             {{ var_nm }} := ref_val)
  }
  
  pbmax <- nrow(df1)
  pb <- txtProgressBar(1, pbmax, width = 50, style = 3)
  
  for (i in 1:nrow(df1)) {
    
    # From df1
    cmr <- df1$patient_record_number[i]
    date <- df1$lab_collection_date[i]
    t1 <- date - 92
    t2 <- date - 1
    src <- case_when(stringr::str_detect(df1$lab_specimen_source[i], regex("capillary", ignore_case = T)) ~ "cap",
                     stringr::str_detect(df1$lab_specimen_source[i], regex("venous", ignore_case = T)) ~ "ven",
                     T ~ "unk")
    
    # From df_lookup (df1 + df2)
    df_lookup <- rbind(df1 %>%
                         select(recno, patient_record_number, starts_with("lab"), test_reason_ks),
                       df2 %>%
                         select(recno, patient_record_number, starts_with("lab"), test_reason_ks))
    all_tests <- df_lookup %>% # All tests within prior 92 days for same individual
      filter(patient_record_number == cmr, lab_collection_date >= t1, lab_collection_date <= t2)
    if (nrow(all_tests) > 0) {
      prev_test <- all_tests %>% # Most recent test within prior 92 days for same individual
        filter(lab_collection_date == max(lab_collection_date))
      prev_rsn <- prev_test$test_reason_ks
      prev_elev <- prev_test$lab_result_elev
      prev_src <- case_when(stringr::str_detect(prev_test$lab_specimen_source, regex("capillary", ignore_case = T)) ~ "cap",
                            stringr::str_detect(prev_test$lab_specimen_source, regex("venous", ignore_case = T)) ~ "ven",
                            T ~ "unk")
    } else {
      prev_test <- all_tests
      prev_rsn <- prev_elev <- prev_src <- NA
    }
    
    rsn <- case_when(nrow(prev_test) == 0 & src == "cap" ~ "cap_screening",
                     nrow(prev_test) == 0 & src == "ven" ~ "ven_confirm_initial",
                     nrow(prev_test) == 1 & src == "ven" & prev_rsn == "cap_screening" & prev_elev == T ~ "ven_confirm_e",
                     nrow(prev_test) == 1 & src == "ven" & prev_rsn == "cap_screening" & prev_elev == F ~ "ven_confirm_n",
                     nrow(prev_test) == 1 & src == "ven" & prev_src == "ven" ~ "ven_followup",
                     T ~ "unknown/other")
    
    df1$test_reason_ks[i] <- rsn
    
    setTxtProgressBar(pb, i)
    # message(i) # If function gets hung, indicates which record
    
  }
  
  close(pb)
  
  df1
  
}



#### classify_test_reason2() - apply version ####

# df1 = data_core for current quarter
# df2 = previous data_core containing test_reason_ks variable

classify_test_reason2 <- function(df1, df2=NULL) {
  
  source("2_functions/func_general.R")
  
  suppressWarnings({
    library(stringr)
    library(dplyr)
  })
  
  var_check(df1, var = c("recno", "hash_value", "patient_record_number", "lab_result_number", "lab_result_elev"))
  df1 <- df1 %>%
    arrange(lab_collection_date, patient_record_number) %>%
    mutate(test_reason_ks = "") # Changed NA to "" for apply version
  
  if (!is.null(df2)) {
    var_check(df2, var = c("recno", "hash_value", "patient_record_number", "lab_result_number", "lab_result_elev", "test_reason_ks"))
    df2 <- df2 %>%
      arrange(lab_collection_date, patient_record_number)
  }
  
  f <- function(r, df2) {
    
    # From df1
    cmr <- r[["patient_record_number"]]
    date <- as.Date(r[["lab_collection_date"]])
    t1 <- date - 92
    t2 <- date - 1
    src <- case_when(stringr::str_detect(r[["lab_specimen_source"]], regex("capillary", ignore_case = T)) ~ "cap",
                     stringr::str_detect(r[["lab_specimen_source"]], regex("venous", ignore_case = T)) ~ "ven",
                     T ~ "unk")
    
    # paste(cmr, date, t1, t2, src)
    
    # From df_lookup (df1 + df2)
    df_lookup <- rbind(df1, df2)
    all_tests <- df_lookup %>% # All tests within prior 92 days for same individual
      filter(patient_record_number == cmr, lab_collection_date >= t1, lab_collection_date <= t2)
    if (nrow(all_tests) > 0) {
      prev_test <- all_tests %>% # Most recent test within prior 92 days for same individual
        filter(lab_collection_date == max(lab_collection_date))
      prev_rsn <- prev_test$test_reason_ks
      prev_elev <- prev_test$lab_result_elev
      prev_src <- case_when(stringr::str_detect(prev_test$lab_specimen_source, regex("capillary", ignore_case = T)) ~ "cap",
                            stringr::str_detect(prev_test$lab_specimen_source, regex("venous", ignore_case = T)) ~ "ven",
                            T ~ "unk")
    } else {
      prev_test <- all_tests
      prev_rsn <- prev_elev <- prev_src <- NA
    }
    
    case_when(nrow(prev_test) == 0 & src == "cap" ~ "cap_screening",
              nrow(prev_test) == 0 & src == "ven" ~ "ven_confirm_initial",
              nrow(prev_test) == 1 & src == "ven" & prev_rsn == "cap_screening" & prev_elev == T ~ "ven_confirm_e",
              nrow(prev_test) == 1 & src == "ven" & prev_rsn == "cap_screening" & prev_elev == F ~ "ven_confirm_n",
              nrow(prev_test) == 1 & src == "ven" & prev_src == "ven" ~ "ven_followup",
              T ~ "unknown/other")
    
  }
  
  apply(df1, 1, f, df2)
  
}



#### clppp_basic_format() ####

clppp_basic_format <- function(df, action) {
  
  actions <- c("add", "change", "delete")
  
  # stopifnot("`action` must be..." = action %in% actions, call. = F)
  if (!action %in% actions) stop("`action` can only be \"add\", \"change\", or \"delete\"", call. = F)
  
  source("2_functions/func_general.R")
  
  var_check(df, var = "lab_collection_date")
  
  suppressWarnings({
    library(dplyr)
  })
  
  y <- get_yr(df, var = "lab_collection_date")
  q <- get_qtr(df, var = "lab_collection_date")
  
  if (length(y) != 1 | length(q) != 1) {
    stop("Data from one quarter of one year is required", call. = F)
  }
  
  data.frame(matrix(nrow = 1, ncol = 0)) %>%
    mutate(ACTION = case_when(action == "add" ~ "A",
                              action == "change" ~ "C",
                              action == "delete" ~ "D"),
           QTR = q,
           RPT_YR = substr(y, 3, 4),
           PGMID = 20007,)
  
}



#### format_lab_table() ####

format_lab_table <- function(df, basic_format, ref_labs) {
  
  source("2_functions/func_general.R")
  
  var_check(df, var = c("recno", "hash_value", "patient_record_number",
                        "lab_result_symbol", "lab_result_number",
                        "pbAddrID", "child_id", "test_reason_ks"))
  var_check(basic_format, var = c("ACTION", "QTR", "RPT_YR", "PGMID"))
  
  if (!all(df$age < 6)) stop("The dataframe should contain only records for children under 6 years old", call. = F)
  
  suppressWarnings({
    library(dplyr)
    library(stringr)
  })
  
  # Add link variables and FILEID
  df_lab <- df %>%
    select(recno, hash_value, patient_record_number) %>%
    mutate(FILEID = "LAB")
  
  # Add basic format variables
  df_lab <- cbind(df_lab, basic_format)
  
  # CHILD_ID (required)
  df_lab$CHILD_ID <- df$child_id
  
  # SAMP_DATE (required)
  df_lab$SAMP_DATE <- format.Date(df$lab_collection_date, "%Y%m%d")
  
  # ADDR_ID (not required)
  df_lab$ADDR_ID <- df$pbAddrID
  
  # PREGNANT (not required)
  df_lab$PREGNANT <- strrep(" ", 1)
  
  # BLANK
  df_lab$BLANK <- strrep(" ", 2)
  
  # LAB_FUND (required)
  df_lab$LAB_FUND <- case_when(str_detect(df$blood_lead_poisoning_form_col_bl_funding_source,
                                          regex("Public", ignore_case = T)) |
                                 !is.na(df$medicaid_id) ~ 1,                                  # 1 – Public, includes Medicaid
                               str_detect(df$blood_lead_poisoning_form_col_bl_funding_source,
                                          regex("Private insurance", ignore_case = T)) ~ 2,   # 2 – Private insurance
                               str_detect(df$blood_lead_poisoning_form_col_bl_funding_source,
                                          regex("Parent self-pay", ignore_case = T)) ~ 3,     # 3 – Parent self-pay
                               str_detect(df$blood_lead_poisoning_form_col_bl_funding_source,
                                          regex("Unknown", ignore_case = T)) ~ 9,             # 9 – Unknown
                               str_detect(df$blood_lead_poisoning_form_col_bl_funding_source,
                                          regex("B|Other|\\w+", ignore_case = T)) ~ 8,        # 8 – Other
                               T ~ 9)                                                         # 9 – Unknown
  
  # SAMP_TYPE (required)
  df_lab$SAMP_TYPE <- case_when(str_detect(df$lab_specimen_source,
                                           regex("venous", ignore_case = T)) |
                                  str_detect(df$lab_specimen_source,
                                             regex("^blood$", ignore_case = T)) ~ 1, # 1 – Venous, blood lead
                                str_detect(df$lab_specimen_source,
                                           regex("capillary", ignore_case = T)) ~ 2, # 2 – Capillary, blood lead
                                T ~ 9)                                               # 9 – Unknown
  
  # TEST_RSN (required)
  df_lab$TEST_RSN <- case_when(df$test_reason_ks == "cap_screening" |
                                 df$test_reason_ks == "ven_confirm_initial" ~ 1, # 1 – Screening (asymptomatic child without previous elevated level)
                               df$test_reason_ks == "ven_confirm_e" ~ 3,         # 3 – Confirmatory test following elevated value by fingerstick
                               df$test_reason_ks == "ven_followup" ~ 4,          # 4 – Follow-up, child with confirmed elevated level
                               T ~ 9)                                            # 9 – Unknown/other
  
  # LAB_TYPE (required)
  df <- df %>%
    left_join(ref_labs, by = "lab_name")
  df_lab$LAB_TYPE <- df$lab_type
  
  # SCRN_SITE (required)
  df_lab$SCRN_SITE <- case_when(grepl(paste0("Susan", "|", "Univ", "|", "Wesley", "|", "Stormo", "|", "Via Chr", 
                                             "|", "providen", "|", "addis", "|", "advent", "|", "affil", 
                                             "|", "atlas", "|", "chc ", "|", "children", "|", "gracemed", 
                                             "|", "ohp ", "|", "memorial", "|", "luke", "|", "cather", 
                                             "|", "\\bst ", "|", "st. ", "|", "swope", "|", "newton", 
                                             "|", "good sa", "|", "mercy", "|", "kvc", "|", "Clara Barton", 
                                             "|", "Regional Hospital", "|", "ATCHISON HOSP", "|", "newman", 
                                             "|", "Mcpherson Hosp", "|", "prairie heal", "|", "prairie star", "|", "Trinity Rock", 
                                             "|", "Central Ks Reg", "|", "Salina Regional", "|", "Salina Health", "|", "Prompt Care", 
                                             "|", "Eckan", "|", "AMS LABORATORIES", "|", "CHILD CARE", "|", "ARCKC", 
                                             "|", "Heartland Programs"),
                                      df$ordering_facility_name, ignore.case = TRUE) ~ 4,
                                grepl(paste0("County", "|", "health dep", "|", "Community", "|", "dist", "|", "primary", 
                                             "|", "smpc", "|", "rodgers", "|", "hunter", "|", " co h", 
                                             "|", "irwin", "|", "kickap", "|", "prairie band", "|", "municipal", 
                                             "|", "Wyco", "|", "tribal", "|", "Munson"),
                                      df$ordering_facility_name, ignore.case = TRUE) ~ 9,
                                grepl(paste0(" MD", "|", " M.D.", "|", " M.D", "|", "family", "|", "System", 
                                             "|", "pediatri", "|", "geriatr", "|", "medical gr", "|", "plains", 
                                             "|", "clinic", "|", "doctor", "|", "cerner", "|", "comcare", 
                                             "|", "Ministries", "|", "Enterprises", "|", "Fam Health", "|", "HEALTHCARE", 
                                             "|", "SASTUN DIREC", "|", "Lucero", "|", "Kansas Pathology", "|", "PARK FAM CARE"), 
                                      df$ordering_facility_name, ignore.case = TRUE) ~ 4,
                                grepl(paste0("heid", "|", "vinzant", "|", "lacey", "|", "charlene", "|", "Hartvickson", "|", "Truong"),
                                      df$ordering_facility_name, ignore.case = TRUE) ~ 4,
                                grepl(paste0("Labette Health", "|", "Hiawatha Com", "|", "City of", "|", "Nemaha Co Comm"),
                                      df$ordering_facility_name, ignore.case = TRUE) ~ 9,
                                TRUE ~ 9)
  
  # METH_ANAZ (required)
  df_lab$METH_ANAZ <- 9 # Unknown
  
  # METH_LOD (not required)
  df_lab$METH_LOD <- strrep(" ", 6)
  
  # SAMP_ANAZ_DT (not required)
  lab_test_date <- format.Date(df$lab_test_date, "%Y%m%d")
  df_lab$SAMP_ANAZ_DT <- case_when(str_detect(lab_test_date, "^\\d{8}$") ~ lab_test_date,
                                   T ~ strrep(" ", 8))
  
  # RSLT_RPT_DT (not required)
  df_lab$RSLT_RPT_DT <- strrep(" ", 8)
  
  # RESULT (required)
  df_lab$RESULT <- str_pad(format(round(df$lab_result_number, digits = 2),
                                  nsmall = 2, trim = T),
                           width = 6, side = "left", pad = "0")
  
  # RST_INTPCODE (required)
  df_lab$RST_INTPCODE <- case_when(is.na(df$lab_result_symbol) ~ 1,           # 1 – Equal
                                   str_detect(df$lab_result_symbol, "<") ~ 2, # 2 – Less Than
                                   str_detect(df$lab_result_symbol, ">") ~ 3) # 3 – Greater Than
  
  # LAB_LOD (not required)
  df_lab$LAB_LOD <- strrep(" ", 6)
  
  # LAB_NAME (not required)
  df_lab$LAB_NAME <- substr(str_pad(toupper(df$lab_name), width = 43, side = "right", pad = " "), 1, 43)
  
  # LAB_ID (not required)
  df_lab$LAB_ID <- strrep(" ", 11)
  
  # NPI (not required)
  df_lab$NPI <- strrep(" ", 10)
  
  df_lab
  
}



#### format_child_table() ####

format_child_table <- function(df, basic_format) {
  
  source("2_functions/func_general.R")
  
  var_check(df, var = c("recno", "hash_value", "patient_record_number",
                        "lab_result_symbol", "lab_result_number",
                        "pbAddrID", "child_id", "test_reason_ks"))
  var_check(basic_format, var = c("ACTION", "QTR", "RPT_YR", "PGMID"))
  
  if (!all(df$age < 6)) stop("The dataframe should contain only records for children under 6 years old", call. = F)
  
  suppressWarnings({
    library(dplyr)
    library(stringr)
  })
  
  df <- df %>%
    distinct(child_id, .keep_all = T)
  
  # Add link variables and FILEID
  df_chi <- df %>%
    select(recno, hash_value, patient_record_number) %>%
    mutate(FILEID = "CHI")
  
  # Add basic format variables
  df_chi <- cbind(df_chi, basic_format)
  
  # CHILD_ID (required)
  df_chi$CHILD_ID <- df$child_id
  
  # DOB (required)
  df_chi$DOB <- format.Date(df$patient_birth_date, "%Y%m%d")
  
  # SEX (required)
  df_chi$SEX <- case_when(str_detect(df$patient_birth_sex, regex("Male", ignore_case = T)) ~ 1,   # 1 – Male
                          str_detect(df$patient_birth_sex, regex("Female", ignore_case = T)) ~ 2, # 2 – Female
                          T ~ 9)                                                                  # 9 – Unknown
  
  # ETHNIC (required)
  df_chi$ETHNIC <- case_when(str_detect(df$patient_ethnicity, regex("^Hispanic", ignore_case = T)) ~ 1,     # 1 – Hispanic or Latino
                             str_detect(df$patient_ethnicity, regex("^Not Hispanic", ignore_case = T)) ~ 2, # 2 – Not Hispanic or Latino
                             T ~ 9)                                                                         # 9 – Unknown
  
  # BLANK
  df_chi$BLANK <- strrep(" ", 1)
  
  # CHELATED (required)
  df_chi$CHELATED <- 2 # No
  
  # CHEL_TYPE (required)
  df_chi$CHEL_TYPE <- strrep(" ", 1)
  
  # CHEL_FUND (required)
  df_chi$CHEL_FUND <- strrep(" ", 1)
  
  # NPLSZ (required)
  df_chi$NPLSZ <- 9 # Unknown
  
  # NPLSM (required)
  df_chi$NPLSM <- 9 # Unknown
  
  # NPLSO (required)
  df_chi$NPLSO <- 9 # Unknown
  
  # NPLSH (required)
  df_chi$NPLSH <- 9 # Unknown
  
  # NPLSP (required)
  df_chi$NPLSP <- 9 # Unknown
  
  # NPLSC (required)
  df_chi$NPLSC <- 9 # Unknown
  
  # BIRTH (not required)
  df_chi$BIRTH <- case_when(str_detect(df$person_country_of_birth, regex("United States", ignore_case = T)) ~ 1, # 1 – U.S.
                            str_detect(df$person_country_of_birth, regex("Unknown", ignore_case = T)) |
                              is.na(df$person_country_of_birth) ~ 3,                                             # 3 – Unknown
                            T ~ 2)                                                                               # 2 – Other
  
  # RACE_AIAN (required)
  df_chi$RACE_AIAN <- case_when(str_detect(df$patient_race, regex("American Indian or Alaska Native", ignore_case = T)) ~ 1, # 1 – Yes
                                T ~ 2)                                                                                       # 2 – No
  
  # RACE_ASIAN (required)
  df_chi$RACE_ASIAN <- case_when(str_detect(df$patient_race, regex("Asian", ignore_case = T)) ~ 1, # 1 – Yes
                                 T ~ 2)                                                            # 2 – No
  
  # RACE_BLACK (required)
  df_chi$RACE_BLACK <- case_when(str_detect(df$patient_race, regex("Black or African American", ignore_case = T)) ~ 1, # 1 – Yes
                                 T ~ 2)                                                                                # 2 – No
  
  # RACE_NHOPI (required)
  df_chi$RACE_NHOPI <- case_when(str_detect(df$patient_race, regex("Native Hawaiian or Other Pacific Islander", ignore_case = T)) ~ 1, # 1 – Yes
                                 T ~ 2)                                                                                                # 2 – No
  
  # RACE_WHITE (required)
  df_chi$RACE_WHITE <- case_when(str_detect(df$patient_race, regex("White", ignore_case = T)) ~ 1, # 1 – Yes
                                 T ~ 2)                                                            # 2 – No
  
  # RACE_OTHER (required)
  df_chi$RACE_OTHER <- case_when(str_detect(df$patient_race, regex("Other", ignore_case = T)) ~ 1, # 1 – Yes
                                 T ~ 2)                                                            # 2 – No
  
  # RACE_RTA (required)
  df_chi$RACE_RTA <- case_when(str_detect(df$patient_race, regex("Refused", ignore_case = T)) ~ 1, # 1 – Yes
                               T ~ 2)                                                              # 2 – No
  
  # RACE_UNK (required)
  df_chi$RACE_UNK <- case_when(str_detect(df$patient_race, regex("Unknown", ignore_case = T)) |
                                 is.na(df$patient_race) ~ 1,                                    # 1 – Yes
                               T ~ 2)                                                           # 2 – No
  
  df_chi
  
}



#### format_address_table() ####

# `df` = data_core_clppp (the DF of all records included in the CLPPP submission)
# `addresses` = df_addresses (the DF of addresses from the submission quarter with `pbAddrID`s)

format_address_table <- function(df, basic_format, addresses) {
  
  source("2_functions/func_general.R")
  
  var_check(df, var = c("recno", "hash_value", "patient_record_number",
                        "lab_result_symbol", "lab_result_number",
                        "pbAddrID", "child_id", "test_reason_ks"))
  var_check(basic_format, var = c("ACTION", "QTR", "RPT_YR", "PGMID"))
  
  if (!all(df$age < 6)) stop("The dataframe should contain only records for children under 6 years old", call. = F)
  
  suppressWarnings({
    library(dplyr)
    library(stringr)
  })
  
  addresses <- addresses %>%
    distinct(pbAddrID, .keep_all = T)
  
  df_add <- df %>%
    select(pbAddrID) %>%
    distinct() %>%
    left_join(addresses, by = "pbAddrID") %>%
    select(ADDR_ID = pbAddrID, CITY = City, CNTY_FIPS = CountyFIPS,
           ZIP = PostalCode, STATE = State, CENSUS = CensusTract) %>%
    mutate(FILEID = "ADD",
           CITY = substr(str_pad(toupper(CITY), width = 15, side = "right", pad = " "), 1, 15),
           CNTY_FIPS = substr(CNTY_FIPS, 3, 5),
           ZIP = str_pad(sub("-", "", ZIP), width = 9, side = "right", pad = " "),
           CENSUS = str_pad(CENSUS, width = 7, side = "right", pad = " "))
  
  # Add basic format variables
  df_add <- cbind(basic_format, df_add)
  
  # RENOVATED (required)
  df_add$RENOVATED <- 9 # Unknown
  
  # START_REN (not required)
  df_add$START_REN <- strrep(" ", 8)
  
  # COMP_REN (not required)
  df_add$COMP_REN <- strrep(" ", 8)
  
  df_add %>%
    relocate(FILEID)
  
}



#### format_investigation_table() ####

format_investigation_table <- function(df, basic_format) {
  
  source("2_functions/func_general.R")
  
  # var_check(df, var = c("recno", "hash_value", "patient_record_number"))
  var_check(basic_format, var = c("ACTION", "QTR", "RPT_YR", "PGMID"))
  
  # if (!all(df$age < 6)) stop("The dataframe should contain only records for children under 6 years old", call. = F)
  
  suppressWarnings({
    library(dplyr)
    library(stringr)
  })
  
  colnames(df) <- str_to_upper(colnames(df))
  
  df_inv <- df %>%
    select(ADDR_ID = PBADDRID, DATE_REF:INDHAZ) %>%
    mutate(FILEID = "INV",
           DATE_REF = gsub("-", "", DATE_REF),
           INSP_COMP = if_else(is.na(INSP_COMP), strrep(" ", 8), gsub("-", "", INSP_COMP)),
           ABAT_COMP = if_else(is.na(ABAT_COMP), strrep(" ", 8), gsub("-", "", ABAT_COMP)),
           YEAR = if_else(is.na(YEAR), strrep(" ", 4), as.character(YEAR)),
           OWNERSHIP = substr(OWNERSHIP, 1, 1),
           DWELL_TYPE = substr(DWELL_TYPE, 1, 1),
           PAINT_HAZ = substr(PAINT_HAZ, 1, 1),
           XRF = str_pad(format(round(as.numeric(sub("<", "", XRF)), digits = 1), nsmall = 1, trim = T),
                         width = 5, side = "left", pad = "0"),
           DUST_FLOOR = str_pad(format(round(as.numeric(sub("<", "", DUST_FLOOR)), digits = 1), nsmall = 1, trim = T),
                                width = 8, side = "left", pad = "0"),
           FLOOR_MSR = case_when(grepl("ug", FLOOR_MSR) ~ "U",
                                 grepl("ppm", FLOOR_MSR) ~ "P",
                                 T ~ " "),
           DUST_SILL = str_pad(format(round(as.numeric(sub("<", "", DUST_SILL)), digits = 1), nsmall = 1, trim = T),
                               width = 8, side = "left", pad = "0"),
           SILL_MSR = case_when(grepl("ug", SILL_MSR) ~ "U",
                                grepl("ppm", SILL_MSR) ~ "P",
                                T ~ " "),
           DUST_WELL = str_pad(format(round(as.numeric(sub("<", "", DUST_WELL)), digits = 1), nsmall = 1, trim = T),
                               width = 8, side = "left", pad = "0"),
           WELL_MSR = case_when(grepl("ug", WELL_MSR) ~ "U",
                                grepl("ppm", WELL_MSR) ~ "P",
                                T ~ " "),
           PAINT = str_pad(format(round(as.numeric(sub("<", "", PAINT)), digits = 1), nsmall = 1, trim = T),
                           width = 8, side = "left", pad = "0"),
           PAINT_MSR = case_when(grepl("ug", PAINT_MSR) ~ "U",
                                 grepl("ppm", PAINT_MSR) ~ "P",
                                 grepl("mg", PAINT_MSR) ~ "M",
                                 T ~ " "),
           SOIL = str_pad(format(round(as.numeric(sub("<", "", SOIL)), digits = 1), nsmall = 1, trim = T),
                          width = 8, side = "left", pad = "0"),
           WATER = str_pad(format(round(as.numeric(sub("<", "", WATER)), digits = 1), nsmall = 1, trim = T),
                           width = 8, side = "left", pad = "0"),
           INDHAZ = substr(INDHAZ, 1, 1),
           DATE_DUE = strrep(" ", 8),
           INV_CLOS_RES = strrep(" ", 1),
           CLEAR_DATE = strrep(" ", 8),
           CLEAR_RSLT = strrep(" ", 1)) %>%
    mutate(XRF = if_else(XRF == "000NA", "000.0", XRF),
           DUST_FLOOR = if_else(DUST_FLOOR == "000000NA", "000000.0", DUST_FLOOR),
           DUST_SILL = if_else(DUST_SILL == "000000NA", "000000.0", DUST_SILL),
           DUST_WELL = if_else(DUST_WELL == "000000NA", "000000.0", DUST_WELL),
           PAINT = if_else(PAINT == "000000NA", "000000.0", PAINT),
           SOIL = if_else(SOIL == "000000NA", "000000.0", SOIL),
           WATER = if_else(WATER == "000000NA", "000000.0", WATER))
  
  # Add basic format variables
  df_inv <- cbind(basic_format, df_inv)
  
  df_inv %>%
    relocate(FILEID)
  
}



#### clppp_check_table() ####

clppp_check_table <- function(df) {
  
  source("2_functions/func_general.R")
  
  var_check(df, var = c("FILEID", "ACTION", "QTR", "RPT_YR", "PGMID"))
  
  suppressWarnings({
    library(dplyr)
    library(stringr)
  })
  
  df <- df %>%
    select(FILEID:last_col())
  
  # Concatenate values in each row
  df <- cbind(df, tidyr::unite(df, col = "all_chars", sep = ""))
  
  # DF of T/F values to catch errors
  df2 <- data.frame(row = 1:nrow(df))

  if (all(df$FILEID == "ADD")) {
    
    var_check(df, var = c("ADDR_ID", "CITY", "CNTY_FIPS", "ZIP", "STATE",
                          "CENSUS", "RENOVATED", "START_REN", "COMP_REN"))
    
    df2$ADDR_ID <- str_detect(df$ADDR_ID, "^\\d{8}$")
    df2$CITY <- str_detect(df$CITY, "^([:alpha:]|\\s){15}$")
    df2$CNTY_FIPS <- str_detect(df$CNTY_FIPS, "^\\d{3}$")
    df2$ZIP <- str_detect(df$ZIP, "^\\d{9}$|^\\d{5}\\s{4}$")
    df2$STATE <- str_detect(df$STATE, "^KS$")
    df2$CENSUS <- str_detect(df$CENSUS, "^\\d{6}\\s$|^\\d{7}$")
    df2$RENOVATED <- str_detect(df$RENOVATED, "^[1239]$")
    df2$START_REN <- str_detect(df$START_REN, "^\\s{8}$")
    df2$COMP_REN <- str_detect(df$COMP_REN, "^\\s{8}$")
    df2$n_chars <- str_detect(df$all_chars, "^.{73}$")
    
  } else if (all(df$FILEID == "CHI")) {
    
    var_check(df, var = c("CHILD_ID", "DOB", "SEX", "ETHNIC", "BLANK",
                          "CHELATED", "CHEL_TYPE", "CHEL_FUND",
                          "NPLSZ", "NPLSM", "NPLSO", "NPLSH", "NPLSP", "NPLSC",
                          "BIRTH", "RACE_AIAN", "RACE_ASIAN", "RACE_BLACK", "RACE_NHOPI",
                          "RACE_WHITE", "RACE_OTHER", "RACE_RTA", "RACE_UNK"))
    
    df2$CHILD_ID <- str_detect(df$CHILD_ID, "^\\d{8}$")
    df2$DOB <- str_detect(df$DOB, "^\\d{8}$")
    df2$SEX <- str_detect(df$SEX, "^[129]$")
    df2$ETHNIC <- str_detect(df$ETHNIC, "^[129]$")
    df2$BLANK <- str_detect(df$BLANK, "^\\s$")
    df2$CHELATED <- str_detect(df$CHELATED, "^[129]$")
    df2$CHEL_TYPE <- str_detect(df$CHEL_TYPE, "^[1239\\s]$")
    df2$CHEL_FUND <- str_detect(df$CHEL_FUND, "^[12389\\s]$")
    df2$NPLSZ <- str_detect(df$NPLSZ, "^[129]$")
    df2$NPLSM <- str_detect(df$NPLSM, "^[129]$")
    df2$NPLSO <- str_detect(df$NPLSO, "^[129]$")
    df2$NPLSH <- str_detect(df$NPLSH, "^[129]$")
    df2$NPLSP <- str_detect(df$NPLSP, "^[129]$")
    df2$NPLSC <- str_detect(df$NPLSC, "^[129]$")
    df2$BIRTH <- str_detect(df$BIRTH, "^[123]$")
    df2$RACE_AIAN <- str_detect(df$RACE_AIAN, "^[12]$")
    df2$RACE_ASIAN <- str_detect(df$RACE_ASIAN, "^[12]$")
    df2$RACE_BLACK <- str_detect(df$RACE_BLACK, "^[12]$")
    df2$RACE_NHOPI <- str_detect(df$RACE_NHOPI, "^[12]$")
    df2$RACE_WHITE <- str_detect(df$RACE_WHITE, "^[12]$")
    df2$RACE_OTHER <- str_detect(df$RACE_OTHER, "^[12]$")
    df2$RACE_RTA <- str_detect(df$RACE_RTA, "^[12]$")
    df2$RACE_UNK <- str_detect(df$RACE_UNK, "^[12]$")
    df2$n_chars <- str_detect(df$all_chars, "^.{49}$")
    
  } else if (all(df$FILEID == "INV")) {
    
    var_check(df, var = c("ADDR_ID", "DATE_REF", "INSP_COMP", "ABAT_COMP", "YEAR", "OWNERSHIP",
                          "DWELL_TYPE", "PAINT_HAZ", "XRF", "DUST_FLOOR", "FLOOR_MSR",
                          "DUST_SILL", "SILL_MSR", "DUST_WELL", "WELL_MSR", "PAINT", "PAINT_MSR",
                          "SOIL", "WATER", "INDHAZ", "DATE_DUE", "INV_CLOS_RES", "CLEAR_DATE", "CLEAR_RSLT"))
    
    df2$ADDR_ID <- str_detect(df$ADDR_ID, "^\\d{8}$")
    df2$DATE_REF <- str_detect(df$DATE_REF, "^\\d{8}$")
    df2$INSP_COMP <- str_detect(df$INSP_COMP, "^\\d{8}$")
    df2$ABAT_COMP <- str_detect(df$ABAT_COMP, "^\\s{8}$")
    df2$YEAR <- str_detect(df$YEAR, "^\\d{4}$")
    df2$OWNERSHIP <- str_detect(df$OWNERSHIP, "^[12349]$")
    df2$DWELL_TYPE <- str_detect(df$DWELL_TYPE, "^[1234589]$")
    df2$PAINT_HAZ <- str_detect(df$PAINT_HAZ, "^[12349]$")
    df2$XRF <- str_detect(df$XRF, "^\\d{3}\\.\\d$")
    df2$DUST_FLOOR <- str_detect(df$DUST_FLOOR, "^\\d{6}\\.\\d$")
    df2$FLOOR_MSR <- str_detect(df$FLOOR_MSR, "^[UP\\s]$")
    df2$DUST_SILL <- str_detect(df$DUST_SILL, "^\\d{6}\\.\\d$")
    df2$SILL_MSR <- str_detect(df$SILL_MSR, "^[UP\\s]$")
    df2$DUST_WELL <- str_detect(df$DUST_WELL, "^\\d{6}\\.\\d$")
    df2$WELL_MSR <- str_detect(df$WELL_MSR, "^[UP\\s]$")
    df2$PAINT <- str_detect(df$PAINT, "^\\d{6}\\.\\d$")
    df2$PAINT_MSR <- str_detect(df$PAINT_MSR, "^[UPM\\s]$")
    df2$SOIL <- str_detect(df$SOIL, "^\\d{6}\\.\\d$")
    df2$WATER <- str_detect(df$WATER, "^\\d{6}\\.\\d$")
    df2$INDHAZ <- str_detect(df$INDHAZ, "^[129]$")
    df2$DATE_DUE <- str_detect(df$DATE_DUE, "^\\s{8}$")
    df2$INV_CLOS_RES <- str_detect(df$INV_CLOS_RES, "^\\s$")
    df2$CLEAR_DATE <- str_detect(df$CLEAR_DATE, "^\\s{8}$")
    df2$CLEAR_RSLT <- str_detect(df$CLEAR_RSLT, "^\\s$")
    df2$n_chars <- str_detect(df$all_chars, "^.{127}$")
    
  } else if (all(df$FILEID == "LAB")) {
    
    var_check(df, var = c("CHILD_ID", "SAMP_DATE", "ADDR_ID", "PREGNANT", "BLANK",
                          "LAB_FUND", "SAMP_TYPE", "TEST_RSN", "LAB_TYPE", "SCRN_SITE",
                          "METH_ANAZ", "METH_LOD", "SAMP_ANAZ_DT", "RSLT_RPT_DT",
                          "RESULT", "RST_INTPCODE", "LAB_LOD", "LAB_NAME", "LAB_ID", "NPI"))
    
    df2$CHILD_ID <- str_detect(df$CHILD_ID, "^\\d{8}$")
    df2$SAMP_DATE <- str_detect(df$SAMP_DATE, "^\\d{8}$")
    df2$ADDR_ID <- str_detect(df$ADDR_ID, "^\\d{8}$")
    df2$PREGNANT <- str_detect(df$PREGNANT, "^\\s$")
    df2$BLANK <- str_detect(df$BLANK, "^\\s{2}$")
    df2$LAB_FUND <- str_detect(df$LAB_FUND, "^[12389]$")
    df2$SAMP_TYPE <- str_detect(df$SAMP_TYPE, "^[129]$")
    df2$TEST_RSN <- str_detect(df$TEST_RSN, "^[123459]$")
    df2$LAB_TYPE <- str_detect(df$LAB_TYPE, "^[1239]$")
    df2$SCRN_SITE <- str_detect(df$SCRN_SITE, "^[123459]$")
    df2$METH_ANAZ <- str_detect(df$METH_ANAZ, "^[1239]$")
    df2$METH_LOD <- str_detect(df$METH_LOD, "^\\s{6}$")
    df2$SAMP_ANAZ_DT <- str_detect(df$SAMP_ANAZ_DT, "^(\\d{8}|\\s{8})$")
    df2$RSLT_RPT_DT <- str_detect(df$RSLT_RPT_DT, "^\\s{8}$")
    df2$RESULT <- str_detect(df$RESULT, "^\\d{3}\\.\\d{2}$")
    df2$RST_INTPCODE <- str_detect(df$RST_INTPCODE, "^[123]$")
    df2$LAB_LOD <- str_detect(df$LAB_LOD, "^\\s{6}$")
    df2$LAB_NAME <- str_detect(df$LAB_NAME, "^[[:graph:]\\s]{43}$")
    df2$LAB_ID <- str_detect(df$LAB_ID, "^\\s{11}$")
    df2$NPI <- str_detect(df$NPI, "^\\s{10}$")
    df2$n_chars <- str_detect(df$all_chars, "^.{144}$")
    
  }
  
  df3 <- df2 %>%
    filter(if_any(everything(), ~ !.x) | if_any(everything(), ~ is.na(.x))) %>%
      select(!where(~all(. == T)))
  
  if (nrow(df3) == 0 & ncol(df3) == 0) {
    message("No errors found")
  } else {
    df3
  }
  
}










