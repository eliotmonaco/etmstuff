cbls_check_table <- function(df) {
  var_check(df, var = c("FILEID", "ACTION", "QTR", "RPT_YR", "PGMID"))

  df <- df %>%
    select(FILEID:last_col())

  # Concatenate values in each row
  df <- cbind(df, tidyr::unite(df, col = "all_chars", sep = ""))

  # Dataframe of T/F values to catch errors
  df2 <- data.frame(row = 1:nrow(df))

  if (all(df$FILEID == "ADD")) {
    # Check address table
    var_check(df, var = c(
      "ADDR_ID", "CITY", "CNTY_FIPS", "ZIP", "STATE",
      "CENSUS", "RENOVATED", "START_REN", "COMP_REN"
    ))
    df2$ADDR_ID <- str_detect(df$ADDR_ID, "^\\d{8}$")
    df2$CITY <- str_detect(df$CITY, "^([:alpha:]|\\s){15}$")
    df2$CNTY_FIPS <- str_detect(df$CNTY_FIPS, "^\\d{3}$")
    df2$ZIP <- str_detect(df$ZIP, "^\\d{9}$|^\\d{5}\\s{4}$")
    df2$STATE <- str_detect(df$STATE, "^KS$")
    df2$CENSUS <- str_detect(df$CENSUS, "^\\d{6}\\s$|^\\d{7}$|^\\s{7}$")
    df2$RENOVATED <- str_detect(df$RENOVATED, "^[1239]$")
    df2$START_REN <- str_detect(df$START_REN, "^\\s{8}$")
    df2$COMP_REN <- str_detect(df$COMP_REN, "^\\s{8}$")
    df2$n_chars <- str_detect(df$all_chars, "^.{73}$")
  } else if (all(df$FILEID == "CHI")) {
    # Check child table
    var_check(df, var = c(
      "CHILD_ID", "DOB", "SEX", "ETHNIC", "BLANK",
      "CHELATED", "CHEL_TYPE", "CHEL_FUND",
      "NPLSZ", "NPLSM", "NPLSO", "NPLSH", "NPLSP", "NPLSC",
      "BIRTH", "RACE_AIAN", "RACE_ASIAN", "RACE_BLACK", "RACE_NHOPI",
      "RACE_WHITE", "RACE_OTHER", "RACE_RTA", "RACE_UNK"
    ))
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
    # Check investigation table
    var_check(df, var = c(
      "ADDR_ID", "DATE_REF", "INSP_COMP", "ABAT_COMP", "YEAR", "OWNERSHIP",
      "DWELL_TYPE", "PAINT_HAZ", "XRF", "DUST_FLOOR", "FLOOR_MSR",
      "DUST_SILL", "SILL_MSR", "DUST_WELL", "WELL_MSR", "PAINT", "PAINT_MSR",
      "SOIL", "WATER", "INDHAZ", "DATE_DUE", "INV_CLOS_RES", "CLEAR_DATE", "CLEAR_RSLT"
    ))
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
    # Check lab table
    var_check(df, var = c(
      "CHILD_ID", "SAMP_DATE", "ADDR_ID", "PREGNANT", "BLANK",
      "LAB_FUND", "SAMP_TYPE", "TEST_RSN", "LAB_TYPE", "SCRN_SITE",
      "METH_ANAZ", "METH_LOD", "SAMP_ANAZ_DT", "RSLT_RPT_DT",
      "RESULT", "RST_INTPCODE", "LAB_LOD", "LAB_NAME", "LAB_ID", "NPI"
    ))
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
    dplyr::filter(
      dplyr::if_any(tidyselect::everything(), ~ !.x) |
        dplyr::if_any(tidyselect::everything(), ~ is.na(.x))
    ) %>%
    dplyr::select(!tidyselect::where(~ all(. == T)))

  if (nrow(df3) == 0 & ncol(df3) == 0) {
    message("No errors found")
  } else {
    df3
  }
}
