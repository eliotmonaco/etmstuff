#' Check a CBLS table for errors
#'
#' This function checks the output of the CBLS table functions ([cbls_address_table()], [cbls_child_table()], [cbls_investigation_table()], and [cbls_lab_table()]). It checks the expected values in each column and the number of characters in each row. If a problem is found, a dataframe containing the affected row and column is returned. If no problems are found, the message `"No problems found"` is returned.
#'
#' @param df A dataframe returned by one of the CBLS table functions.
#'
#' @return A dataframe containing errors in `df` or a message that no errors were found.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
# @examples
#'
cbls_check_table <- function(df) {
  vars_key <- c("FILEID", "ACTION", "QTR", "RPT_YR", "PGMID")

  var_check(df, var = vars_key)

  # Keep CBLS table variables and add `all_chars` to check character count
  df <- df %>%
    dplyr::select("FILEID":tidyselect::last_col()) %>%
    tidyr::unite(
      col = "all_chars",
      tidyselect::everything(),
      sep = "",
      remove = FALSE,
      na.rm = FALSE
    ) %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    dplyr::relocate("row")

  if (all(df$FILEID == "ADD")) { # Check Address table
    var_check(df, var = c(
      "ADDR_ID", "CITY", "CNTY_FIPS", "ZIP", "STATE",
      "CENSUS", "RENOVATED", "START_REN", "COMP_REN"
    ))
    df <- df %>%
      dplyr::mutate(
        ADDR_ID = stringr::str_detect(.data$ADDR_ID, "^\\d{8}$"),
        CITY = stringr::str_detect(.data$CITY, "^([:alpha:]|\\s){15}$"),
        CNTY_FIPS = stringr::str_detect(.data$CNTY_FIPS, "^\\d{3}$"),
        ZIP = stringr::str_detect(.data$ZIP, "^\\d{9}$|^\\d{5}\\s{4}$"),
        STATE = stringr::str_detect(.data$STATE, "^KS$"),
        CENSUS = stringr::str_detect(.data$CENSUS, "^\\d{6}\\s$|^\\d{7}$|^\\s{7}$"),
        RENOVATED = stringr::str_detect(.data$RENOVATED, "^[1239]$"),
        START_REN = stringr::str_detect(.data$START_REN, "^\\s{8}$"),
        COMP_REN = stringr::str_detect(.data$COMP_REN, "^\\s{8}$"),
        n_chars = stringr::str_detect(.data$all_chars, "^.{73}$")
      )
  } else if (all(df$FILEID == "CHI")) { # Check Child table
    var_check(df, var = c(
      "CHILD_ID", "DOB", "SEX", "ETHNIC", "BLANK",
      "CHELATED", "CHEL_TYPE", "CHEL_FUND",
      "NPLSZ", "NPLSM", "NPLSO", "NPLSH", "NPLSP", "NPLSC",
      "BIRTH", "RACE_AIAN", "RACE_ASIAN", "RACE_BLACK", "RACE_NHOPI",
      "RACE_WHITE", "RACE_OTHER", "RACE_RTA", "RACE_UNK"
    ))
    df <- df %>%
      dplyr::mutate(
        CHILD_ID = stringr::str_detect(.data$CHILD_ID, "^\\d{8}$"),
        DOB = stringr::str_detect(.data$DOB, "^\\d{8}$"),
        SEX = stringr::str_detect(.data$SEX, "^[129]$"),
        ETHNIC = stringr::str_detect(.data$ETHNIC, "^[129]$"),
        BLANK = stringr::str_detect(.data$BLANK, "^\\s$"),
        CHELATED = stringr::str_detect(.data$CHELATED, "^[129]$"),
        CHEL_TYPE = stringr::str_detect(.data$CHEL_TYPE, "^[1239\\s]$"),
        CHEL_FUND = stringr::str_detect(.data$CHEL_FUND, "^[12389\\s]$"),
        NPLSZ = stringr::str_detect(.data$NPLSZ, "^[129]$"),
        NPLSM = stringr::str_detect(.data$NPLSM, "^[129]$"),
        NPLSO = stringr::str_detect(.data$NPLSO, "^[129]$"),
        NPLSH = stringr::str_detect(.data$NPLSH, "^[129]$"),
        NPLSP = stringr::str_detect(.data$NPLSP, "^[129]$"),
        NPLSC = stringr::str_detect(.data$NPLSC, "^[129]$"),
        BIRTH = stringr::str_detect(.data$BIRTH, "^[123]$"),
        RACE_AIAN = stringr::str_detect(.data$RACE_AIAN, "^[12]$"),
        RACE_ASIAN = stringr::str_detect(.data$RACE_ASIAN, "^[12]$"),
        RACE_BLACK = stringr::str_detect(.data$RACE_BLACK, "^[12]$"),
        RACE_NHOPI = stringr::str_detect(.data$RACE_NHOPI, "^[12]$"),
        RACE_WHITE = stringr::str_detect(.data$RACE_WHITE, "^[12]$"),
        RACE_OTHER = stringr::str_detect(.data$RACE_OTHER, "^[12]$"),
        RACE_RTA = stringr::str_detect(.data$RACE_RTA, "^[12]$"),
        RACE_UNK = stringr::str_detect(.data$RACE_UNK, "^[12]$"),
        n_chars = stringr::str_detect(.data$all_chars, "^.{49}$")
      )
  } else if (all(df$FILEID == "INV")) { # Check Investigation table
    var_check(df, var = c(
      "ADDR_ID", "DATE_REF", "INSP_COMP", "ABAT_COMP", "YEAR", "OWNERSHIP",
      "DWELL_TYPE", "PAINT_HAZ", "XRF", "DUST_FLOOR", "FLOOR_MSR",
      "DUST_SILL", "SILL_MSR", "DUST_WELL", "WELL_MSR", "PAINT", "PAINT_MSR",
      "SOIL", "WATER", "INDHAZ", "DATE_DUE", "INV_CLOS_RES", "CLEAR_DATE", "CLEAR_RSLT"
    ))
    df <- df %>%
      dplyr::mutate(
        ADDR_ID = stringr::str_detect(.data$ADDR_ID, "^\\d{8}$"),
        DATE_REF = stringr::str_detect(.data$DATE_REF, "^\\d{8}$"),
        INSP_COMP = stringr::str_detect(.data$INSP_COMP, "^\\d{8}$"),
        ABAT_COMP = stringr::str_detect(.data$ABAT_COMP, "^\\s{8}$"),
        YEAR = stringr::str_detect(.data$YEAR, "^\\d{4}$"),
        OWNERSHIP = stringr::str_detect(.data$OWNERSHIP, "^[12349]$"),
        DWELL_TYPE = stringr::str_detect(.data$DWELL_TYPE, "^[1234589]$"),
        PAINT_HAZ = stringr::str_detect(.data$PAINT_HAZ, "^[12349]$"),
        XRF = stringr::str_detect(.data$XRF, "^\\d{3}\\.\\d$"),
        DUST_FLOOR = stringr::str_detect(.data$DUST_FLOOR, "^\\d{6}\\.\\d$"),
        FLOOR_MSR = stringr::str_detect(.data$FLOOR_MSR, "^[UP\\s]$"),
        DUST_SILL = stringr::str_detect(.data$DUST_SILL, "^\\d{6}\\.\\d$"),
        SILL_MSR = stringr::str_detect(.data$SILL_MSR, "^[UP\\s]$"),
        DUST_WELL = stringr::str_detect(.data$DUST_WELL, "^\\d{6}\\.\\d$"),
        WELL_MSR = stringr::str_detect(.data$WELL_MSR, "^[UP\\s]$"),
        PAINT = stringr::str_detect(.data$PAINT, "^\\d{6}\\.\\d$"),
        PAINT_MSR = stringr::str_detect(.data$PAINT_MSR, "^[UPM\\s]$"),
        SOIL = stringr::str_detect(.data$SOIL, "^\\d{6}\\.\\d$"),
        WATER = stringr::str_detect(.data$WATER, "^\\d{6}\\.\\d$"),
        INDHAZ = stringr::str_detect(.data$INDHAZ, "^[129]$"),
        DATE_DUE = stringr::str_detect(.data$DATE_DUE, "^\\s{8}$"),
        INV_CLOS_RES = stringr::str_detect(.data$INV_CLOS_RES, "^\\s$"),
        CLEAR_DATE = stringr::str_detect(.data$CLEAR_DATE, "^\\s{8}$"),
        CLEAR_RSLT = stringr::str_detect(.data$CLEAR_RSLT, "^\\s$"),
        n_chars = stringr::str_detect(.data$all_chars, "^.{127}$")
      )
  } else if (all(df$FILEID == "LAB")) { # Check Lab Results table
    var_check(df, var = c(
      "CHILD_ID", "SAMP_DATE", "ADDR_ID", "PREGNANT", "BLANK",
      "LAB_FUND", "SAMP_TYPE", "TEST_RSN", "LAB_TYPE", "SCRN_SITE",
      "METH_ANAZ", "METH_LOD", "SAMP_ANAZ_DT", "RSLT_RPT_DT",
      "RESULT", "RST_INTPCODE", "LAB_LOD", "LAB_NAME", "LAB_ID", "NPI"
    ))
    df <- df %>%
      dplyr::mutate(
        CHILD_ID = stringr::str_detect(.data$CHILD_ID, "^\\d{8}$"),
        SAMP_DATE = stringr::str_detect(.data$SAMP_DATE, "^\\d{8}$"),
        ADDR_ID = stringr::str_detect(.data$ADDR_ID, "^\\d{8}$"),
        PREGNANT = stringr::str_detect(.data$PREGNANT, "^\\s$"),
        BLANK = stringr::str_detect(.data$BLANK, "^\\s{2}$"),
        LAB_FUND = stringr::str_detect(.data$LAB_FUND, "^[12389]$"),
        SAMP_TYPE = stringr::str_detect(.data$SAMP_TYPE, "^[129]$"),
        TEST_RSN = stringr::str_detect(.data$TEST_RSN, "^[123459]$"),
        LAB_TYPE = stringr::str_detect(.data$LAB_TYPE, "^[1239]$"),
        SCRN_SITE = stringr::str_detect(.data$SCRN_SITE, "^[123459]$"),
        METH_ANAZ = stringr::str_detect(.data$METH_ANAZ, "^[1239]$"),
        METH_LOD = stringr::str_detect(.data$METH_LOD, "^\\d{3}\\.\\d{2}$"),
        SAMP_ANAZ_DT = stringr::str_detect(.data$SAMP_ANAZ_DT, "^(\\d{8}|\\s{8})$"),
        RSLT_RPT_DT = stringr::str_detect(.data$RSLT_RPT_DT, "^\\s{8}$"),
        RESULT = stringr::str_detect(.data$RESULT, "^\\d{3}\\.\\d{2}$"),
        RST_INTPCODE = stringr::str_detect(.data$RST_INTPCODE, "^[123]$"),
        LAB_LOD = stringr::str_detect(.data$LAB_LOD, "^\\d{3}\\.\\d{2}$"),
        LAB_NAME = stringr::str_detect(.data$LAB_NAME, "^[[:graph:]\\s]{43}$"),
        LAB_ID = stringr::str_detect(.data$LAB_ID, "^\\s{11}$"),
        NPI = stringr::str_detect(.data$NPI, "^\\s{10}$"),
        n_chars = stringr::str_detect(.data$all_chars, "^.{144}$")
      )
  }

  df <- df %>%
    dplyr::select(-tidyselect::all_of(vars_key), -"all_chars")

  # Replace NA with FALSE
  df[is.na(df)] <- FALSE

  # Keep rows and columns with a value of FALSE
  df <- df %>%
    dplyr::filter(dplyr::if_any(tidyselect::everything(), ~ !.x)) %>%
    dplyr::select(!tidyselect::where(~ all(. == TRUE)))

  if (nrow(df) == 0 & ncol(df) == 0) {
    message("No errors found")
  } else {
    df
  }
}
