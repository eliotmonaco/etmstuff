
#### postmastr_compare() ####

postmastr_compare <- function(df, ref1, ref2) {

  var_check(df, var = c("address_id", "street_unit", "pm.address"))

  suppressWarnings({
    library(dplyr)
  })

  p1 <- setNames(ref1$replacement, paste0("(?<=^\\d{1,7}\\s)", ref1$pattern, "(?=\\s)"))
  p2 <- setNames(ref2$replacement, paste0("(?<=\\s)", ref2$pattern, "$"))

  df$new <- stringr::str_replace_all(df$street_unit, stringr::regex(p1, ignore_case = TRUE))
  df$new <- stringr::str_replace_all(df$new, stringr::regex(p2, ignore_case = TRUE))

  df %>%
    filter(stringr::str_to_upper(new) != stringr::str_to_upper(street_geo)) %>%
    select(-new)

}



#### explore_md_results() ####

explore_md_results <- function(df, ref) {

  source("2_functions/func_general.R")

  var_check(df, var = c("MelissaAddressKey", "Results"))

  suppressWarnings({
    library(dplyr)
  })

  c <- ncol(ref) + 1
  vars_ref <- colnames(ref)[-1]

  # Split result codes into separate columns
  md_results <- as.data.frame(stringr::str_split(df$Results, ",", simplify = T))

  # Count result codes in each column of md_results and join to ref
  for (i in 1:ncol(md_results)) {
    col <- paste0("V", i)
    df_count <- md_results %>%
      group_by(!!as.name(col)) %>%
      count()
    colnames(df_count)[1] <- "Code"
    ref <- ref %>%
      left_join(df_count, by = "Code")
  }

  # Sum counts, remove count of blank values, and sort by code
  ref$Count <- apply(ref[c:(c + i - 1)], 1, sum, na.rm = T)
  ref <- ref %>%
    select(Code, Count, all_of(vars_ref))

  df_md_AE02 <- df %>%
    filter(stringr::str_detect(Results, "AE02"))

  df_md_AS13 <- df %>%
    filter(stringr::str_detect(Results, "AS13"))

  df_md_AC02 <- df %>%
    filter(stringr::str_detect(Results, "AC02"))

  df_md_AC20 <- df %>%
    filter(stringr::str_detect(Results, "AC20"))

  df_md_AS23 <- df %>%
    filter(stringr::str_detect(Results, "AS23"))

  list(df_md_codes_ct = ref,
       df_md_AE02 = df_md_AE02,
       df_md_AS13 = df_md_AS13,
       df_md_AC02 = df_md_AC02,
       df_md_AC20 = df_md_AC20,
       df_md_AS23 = df_md_AS23)

}



#### clppp_flag() ####

# Outputs a T/F column indicating whether or not the address is eligible to be
# included in the CDC submission.

clppp_flag <- function(df) {

  source("2_functions/func_general.R")

  var_check(df, var = c("DeliveryIndicator", "MelissaAddressKey", "Results", "State"))

  f <- function(r) {
    if (
      # Results must contain one of...
      stringr::str_detect(r[["Results"]], "AS(01|02|03|12|13|16|17|24)|AE06") &
      # Results can't contain one of...
      !stringr::str_detect(r[["Results"]], "AE(01|02|03|04|05|07|10|11|12|13|14)|AS(09|10|11|20|23)") &
      # Can't be a business address
      !stringr::str_detect(r[["DeliveryIndicator"]], "B") &
      # Must be in Kansas
      stringr::str_detect(r[["State"]], "KS")) {
      T
    } else {
      F
    }
  }

  apply(df, 1, f)

}



#### flag_addresses() ####

flag_addresses <- function(df) {

  source("2_functions/func_general.R")

  var_check(df, var = c("DeliveryIndicator", "MelissaAddressKey", "Results", "State"))

  df$NoUnit_Flag <- stringr::str_detect(df$Results, "AE(08|09)")

  df$PCM_Flag <- stringr::str_detect(df$Results, "AS(10|11|20)")

  df$EWS_Flag <- stringr::str_detect(df$Results, "AE06")

  df$NoUSPS_Flag <- stringr::str_detect(df$Results, "AS03")

  df$State_Flag <- stringr::str_detect(df$Results, "AC02")

  df$Foreign_Flag <- stringr::str_detect(df$Results, "AS09")

  df$NoMAK_Flag <- !stringr::str_detect(df$MelissaAddressKey, "\\d{10}")

  df$Bus_Flag <- stringr::str_detect(df$DeliveryIndicator, "B")

  df$ValidAddr_Flag <- !stringr::str_detect(df$Results, "AE(01|02|03|04|05|07|10|11|12|13|14)|AS23")

  df$CLPPP_Flag <- clppp_flag(df)

  df$GeoType <- stringr::str_extract(df$Results, "GS\\d{2}")

  df

}



