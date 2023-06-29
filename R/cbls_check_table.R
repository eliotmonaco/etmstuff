#' Check a CBLS table
#'
#' This function checks the output of the CBLS table functions ([cbls_address_table()], [cbls_child_table()], [cbls_investigation_table()], and [cbls_lab_table()]). It checks the expected values in each column and the number of characters in each row. If a problem is found, a dataframe containing the affected row and column is returned. If no problems are found, the message `"No problems found"` is returned.
#'
#' @param df A dataframe returned by one of the CBLS table functions.
#'
#' @return A dataframe or message, depending on the content of `df`.
#' @export
#'
#' @importFrom magrittr %>%
#'
# @examples
#'
cbls_check_table <- function(df) {
  var_check(df, var = c("FILEID", "ACTION", "QTR", "RPT_YR", "PGMID"))

  # Keep CBLS table variables and add `all_chars` to check character count
  df <- df %>%
    dplyr::select(FILEID:tidyselect::last_col()) %>%
    tidyr::unite(
      col = "all_chars",
      tidyselect::everything(),
      sep = "",
      remove = FALSE,
      na.rm = FALSE
    )

  # Dataframe of T/F values to catch errors
  df2 <- data.frame(row = 1:nrow(df))

  if (all(df$FILEID == "ADD")) {
    # Check address table
    var_check(df, var = c(
      "ADDR_ID", "CITY", "CNTY_FIPS", "ZIP", "STATE",
      "CENSUS", "RENOVATED", "START_REN", "COMP_REN"
    ))
    df2$ADDR_ID <- stringr::str_detect(df$ADDR_ID, "^\\d{8}$")
    df2$CITY <- stringr::str_detect(df$CITY, "^([:alpha:]|\\s){15}$")
    df2$CNTY_FIPS <- stringr::str_detect(df$CNTY_FIPS, "^\\d{3}$")
    df2$ZIP <- stringr::str_detect(df$ZIP, "^\\d{9}$|^\\d{5}\\s{4}$")
    df2$STATE <- stringr::str_detect(df$STATE, "^KS$")
    df2$CENSUS <- stringr::str_detect(df$CENSUS, "^\\d{6}\\s$|^\\d{7}$|^\\s{7}$")
    df2$RENOVATED <- stringr::str_detect(df$RENOVATED, "^[1239]$")
    df2$START_REN <- stringr::str_detect(df$START_REN, "^\\s{8}$")
    df2$COMP_REN <- stringr::str_detect(df$COMP_REN, "^\\s{8}$")
    df2$n_chars <- stringr::str_detect(df$all_chars, "^.{73}$")
  } else if (all(df$FILEID == "CHI")) {
    # Check child table
    var_check(df, var = c(
      "CHILD_ID", "DOB", "SEX", "ETHNIC", "BLANK",
      "CHELATED", "CHEL_TYPE", "CHEL_FUND",
      "NPLSZ", "NPLSM", "NPLSO", "NPLSH", "NPLSP", "NPLSC",
      "BIRTH", "RACE_AIAN", "RACE_ASIAN", "RACE_BLACK", "RACE_NHOPI",
      "RACE_WHITE", "RACE_OTHER", "RACE_RTA", "RACE_UNK"
    ))
    df2$CHILD_ID <- stringr::str_detect(df$CHILD_ID, "^\\d{8}$")
    df2$DOB <- stringr::str_detect(df$DOB, "^\\d{8}$")
    df2$SEX <- stringr::str_detect(df$SEX, "^[129]$")
    df2$ETHNIC <- stringr::str_detect(df$ETHNIC, "^[129]$")
    df2$BLANK <- stringr::str_detect(df$BLANK, "^\\s$")
    df2$CHELATED <- stringr::str_detect(df$CHELATED, "^[129]$")
    df2$CHEL_TYPE <- stringr::str_detect(df$CHEL_TYPE, "^[1239\\s]$")
    df2$CHEL_FUND <- stringr::str_detect(df$CHEL_FUND, "^[12389\\s]$")
    df2$NPLSZ <- stringr::str_detect(df$NPLSZ, "^[129]$")
    df2$NPLSM <- stringr::str_detect(df$NPLSM, "^[129]$")
    df2$NPLSO <- stringr::str_detect(df$NPLSO, "^[129]$")
    df2$NPLSH <- stringr::str_detect(df$NPLSH, "^[129]$")
    df2$NPLSP <- stringr::str_detect(df$NPLSP, "^[129]$")
    df2$NPLSC <- stringr::str_detect(df$NPLSC, "^[129]$")
    df2$BIRTH <- stringr::str_detect(df$BIRTH, "^[123]$")
    df2$RACE_AIAN <- stringr::str_detect(df$RACE_AIAN, "^[12]$")
    df2$RACE_ASIAN <- stringr::str_detect(df$RACE_ASIAN, "^[12]$")
    df2$RACE_BLACK <- stringr::str_detect(df$RACE_BLACK, "^[12]$")
    df2$RACE_NHOPI <- stringr::str_detect(df$RACE_NHOPI, "^[12]$")
    df2$RACE_WHITE <- stringr::str_detect(df$RACE_WHITE, "^[12]$")
    df2$RACE_OTHER <- stringr::str_detect(df$RACE_OTHER, "^[12]$")
    df2$RACE_RTA <- stringr::str_detect(df$RACE_RTA, "^[12]$")
    df2$RACE_UNK <- stringr::str_detect(df$RACE_UNK, "^[12]$")
    df2$n_chars <- stringr::str_detect(df$all_chars, "^.{49}$")
  } else if (all(df$FILEID == "INV")) {
    # Check investigation table
    var_check(df, var = c(
      "ADDR_ID", "DATE_REF", "INSP_COMP", "ABAT_COMP", "YEAR", "OWNERSHIP",
      "DWELL_TYPE", "PAINT_HAZ", "XRF", "DUST_FLOOR", "FLOOR_MSR",
      "DUST_SILL", "SILL_MSR", "DUST_WELL", "WELL_MSR", "PAINT", "PAINT_MSR",
      "SOIL", "WATER", "INDHAZ", "DATE_DUE", "INV_CLOS_RES", "CLEAR_DATE", "CLEAR_RSLT"
    ))
    df2$ADDR_ID <- stringr::str_detect(df$ADDR_ID, "^\\d{8}$")
    df2$DATE_REF <- stringr::str_detect(df$DATE_REF, "^\\d{8}$")
    df2$INSP_COMP <- stringr::str_detect(df$INSP_COMP, "^\\d{8}$")
    df2$ABAT_COMP <- stringr::str_detect(df$ABAT_COMP, "^\\s{8}$")
    df2$YEAR <- stringr::str_detect(df$YEAR, "^\\d{4}$")
    df2$OWNERSHIP <- stringr::str_detect(df$OWNERSHIP, "^[12349]$")
    df2$DWELL_TYPE <- stringr::str_detect(df$DWELL_TYPE, "^[1234589]$")
    df2$PAINT_HAZ <- stringr::str_detect(df$PAINT_HAZ, "^[12349]$")
    df2$XRF <- stringr::str_detect(df$XRF, "^\\d{3}\\.\\d$")
    df2$DUST_FLOOR <- stringr::str_detect(df$DUST_FLOOR, "^\\d{6}\\.\\d$")
    df2$FLOOR_MSR <- stringr::str_detect(df$FLOOR_MSR, "^[UP\\s]$")
    df2$DUST_SILL <- stringr::str_detect(df$DUST_SILL, "^\\d{6}\\.\\d$")
    df2$SILL_MSR <- stringr::str_detect(df$SILL_MSR, "^[UP\\s]$")
    df2$DUST_WELL <- stringr::str_detect(df$DUST_WELL, "^\\d{6}\\.\\d$")
    df2$WELL_MSR <- stringr::str_detect(df$WELL_MSR, "^[UP\\s]$")
    df2$PAINT <- stringr::str_detect(df$PAINT, "^\\d{6}\\.\\d$")
    df2$PAINT_MSR <- stringr::str_detect(df$PAINT_MSR, "^[UPM\\s]$")
    df2$SOIL <- stringr::str_detect(df$SOIL, "^\\d{6}\\.\\d$")
    df2$WATER <- stringr::str_detect(df$WATER, "^\\d{6}\\.\\d$")
    df2$INDHAZ <- stringr::str_detect(df$INDHAZ, "^[129]$")
    df2$DATE_DUE <- stringr::str_detect(df$DATE_DUE, "^\\s{8}$")
    df2$INV_CLOS_RES <- stringr::str_detect(df$INV_CLOS_RES, "^\\s$")
    df2$CLEAR_DATE <- stringr::str_detect(df$CLEAR_DATE, "^\\s{8}$")
    df2$CLEAR_RSLT <- stringr::str_detect(df$CLEAR_RSLT, "^\\s$")
    df2$n_chars <- stringr::str_detect(df$all_chars, "^.{127}$")
  } else if (all(df$FILEID == "LAB")) {
    # Check lab table
    var_check(df, var = c(
      "CHILD_ID", "SAMP_DATE", "ADDR_ID", "PREGNANT", "BLANK",
      "LAB_FUND", "SAMP_TYPE", "TEST_RSN", "LAB_TYPE", "SCRN_SITE",
      "METH_ANAZ", "METH_LOD", "SAMP_ANAZ_DT", "RSLT_RPT_DT",
      "RESULT", "RST_INTPCODE", "LAB_LOD", "LAB_NAME", "LAB_ID", "NPI"
    ))
    df2$CHILD_ID <- stringr::str_detect(df$CHILD_ID, "^\\d{8}$")
    df2$SAMP_DATE <- stringr::str_detect(df$SAMP_DATE, "^\\d{8}$")
    df2$ADDR_ID <- stringr::str_detect(df$ADDR_ID, "^\\d{8}$")
    df2$PREGNANT <- stringr::str_detect(df$PREGNANT, "^\\s$")
    df2$BLANK <- stringr::str_detect(df$BLANK, "^\\s{2}$")
    df2$LAB_FUND <- stringr::str_detect(df$LAB_FUND, "^[12389]$")
    df2$SAMP_TYPE <- stringr::str_detect(df$SAMP_TYPE, "^[129]$")
    df2$TEST_RSN <- stringr::str_detect(df$TEST_RSN, "^[123459]$")
    df2$LAB_TYPE <- stringr::str_detect(df$LAB_TYPE, "^[1239]$")
    df2$SCRN_SITE <- stringr::str_detect(df$SCRN_SITE, "^[123459]$")
    df2$METH_ANAZ <- stringr::str_detect(df$METH_ANAZ, "^[1239]$")
    df2$METH_LOD <- stringr::str_detect(df$METH_LOD, "^\\s{6}$")
    df2$SAMP_ANAZ_DT <- stringr::str_detect(df$SAMP_ANAZ_DT, "^(\\d{8}|\\s{8})$")
    df2$RSLT_RPT_DT <- stringr::str_detect(df$RSLT_RPT_DT, "^\\s{8}$")
    df2$RESULT <- stringr::str_detect(df$RESULT, "^\\d{3}\\.\\d{2}$")
    df2$RST_INTPCODE <- stringr::str_detect(df$RST_INTPCODE, "^[123]$")
    df2$LAB_LOD <- stringr::str_detect(df$LAB_LOD, "^\\s{6}$")
    df2$LAB_NAME <- stringr::str_detect(df$LAB_NAME, "^[[:graph:]\\s]{43}$")
    df2$LAB_ID <- stringr::str_detect(df$LAB_ID, "^\\s{11}$")
    df2$NPI <- stringr::str_detect(df$NPI, "^\\s{10}$")
    df2$n_chars <- stringr::str_detect(df$all_chars, "^.{144}$")
  }

  # Replace NA with FALSE
  df2[is.na(df2)] <- FALSE

  # Keep rows and columns with a value of FALSE
  df2 <- df2 %>%
    dplyr::filter(dplyr::if_any(tidyselect::everything(), ~ !.x)) %>%
    dplyr::select(!tidyselect::where(~ all(. == T)))

  if (nrow(df2) == 0 & ncol(df2) == 0) {
    message("No errors found")
  } else {
    df2
  }
}
