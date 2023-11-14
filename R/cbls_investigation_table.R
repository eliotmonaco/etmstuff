#' Create a CBLS Investigation table
#'
#' @param df A dataframe of investigation records.
#' @param key A dataframe returned by [cbls_table_key()].
#'
#' @return A dataframe formatted as an Investigation table per CBLS guidelines.
#' @export
#'
#' @importFrom magrittr %>%
#'
# @examples
#'
cbls_investigation_table <- function(df, key) {
  vars_inv <- c(
    "addr_id", "date_ref", "insp_comp", "abat_comp", "year",
    "ownership", "dwell_type", "paint_haz", "xrf",
    "dust_floor", "floor_msr", "dust_sill", "sill_msr",
    "dust_well", "well_msr", "paint", "paint_msr", "soil",
    "water", "indhaz"
  )

  var_check(df, var = vars_inv)
  var_check(key, var = c("ACTION", "QTR", "RPT_YR", "PGMID"))

  colnames(df) <- stringr::str_to_upper(colnames(df))

  df_inv <- df %>%
    dplyr::mutate(
      FILEID = "INV",
      DATE_REF = gsub("-", "", DATE_REF),
      INSP_COMP = dplyr::if_else(
        is.na(INSP_COMP),
        true = strrep(" ", 8),
        false = gsub("-", "", INSP_COMP)
      ),
      ABAT_COMP = dplyr::if_else(
        is.na(ABAT_COMP),
        true = strrep(" ", 8),
        false = gsub("-", "", ABAT_COMP)
      ),
      YEAR = dplyr::if_else(
        is.na(YEAR),
        true = strrep(" ", 4),
        false = as.character(YEAR)
      ),
      OWNERSHIP = substr(OWNERSHIP, 1, 1),
      DWELL_TYPE = substr(DWELL_TYPE, 1, 1),
      PAINT_HAZ = substr(PAINT_HAZ, 1, 1),
      XRF = stringr::str_pad(
        format(round(as.numeric(sub("<", "", XRF)), digits = 1), nsmall = 1, trim = TRUE),
        width = 5, side = "left", pad = "0"
      ),
      DUST_FLOOR = stringr::str_pad(
        format(round(as.numeric(sub("<", "", DUST_FLOOR)), digits = 1), nsmall = 1, trim = TRUE),
        width = 8, side = "left", pad = "0"
      ),
      FLOOR_MSR = dplyr::case_when(
        grepl("ug", FLOOR_MSR) ~ "U",
        grepl("ppm", FLOOR_MSR) ~ "P",
        TRUE ~ " "
      ),
      DUST_SILL = stringr::str_pad(
        format(round(as.numeric(sub("<", "", DUST_SILL)), digits = 1), nsmall = 1, trim = TRUE),
        width = 8, side = "left", pad = "0"
      ),
      SILL_MSR = dplyr::case_when(
        grepl("ug", SILL_MSR) ~ "U",
        grepl("ppm", SILL_MSR) ~ "P",
        TRUE ~ " "
      ),
      DUST_WELL = stringr::str_pad(
        format(round(as.numeric(sub("<", "", DUST_WELL)), digits = 1), nsmall = 1, trim = TRUE),
        width = 8, side = "left", pad = "0"
      ),
      WELL_MSR = dplyr::case_when(
        grepl("ug", WELL_MSR) ~ "U",
        grepl("ppm", WELL_MSR) ~ "P",
        TRUE ~ " "
      ),
      PAINT = stringr::str_pad(
        format(round(as.numeric(sub("<", "", PAINT)), digits = 1), nsmall = 1, trim = TRUE),
        width = 8, side = "left", pad = "0"
      ),
      PAINT_MSR = dplyr::case_when(
        grepl("ug", PAINT_MSR) ~ "U",
        grepl("ppm", PAINT_MSR) ~ "P",
        grepl("mg", PAINT_MSR) ~ "M",
        TRUE ~ " "
      ),
      SOIL = stringr::str_pad(
        format(round(as.numeric(sub("<", "", SOIL)), digits = 1), nsmall = 1, trim = TRUE),
        width = 8, side = "left", pad = "0"
      ),
      WATER = stringr::str_pad(
        format(round(as.numeric(sub("<", "", WATER)), digits = 1), nsmall = 1, trim = TRUE),
        width = 8, side = "left", pad = "0"
      ),
      INDHAZ = substr(INDHAZ, 1, 1),
      DATE_DUE = strrep(" ", 8),
      INV_CLOS_RES = strrep(" ", 1),
      CLEAR_DATE = strrep(" ", 8),
      CLEAR_RSLT = strrep(" ", 1)
    ) %>%
    dplyr::mutate(
      XRF = dplyr::if_else(
        XRF == "000NA",
        true = "000.0",
        false = XRF
      ),
      DUST_FLOOR = dplyr::if_else(
        DUST_FLOOR == "000000NA",
        true = "000000.0",
        false = DUST_FLOOR
      ),
      DUST_SILL = dplyr::if_else(
        DUST_SILL == "000000NA",
        true = "000000.0",
        false = DUST_SILL
      ),
      DUST_WELL = dplyr::if_else(
        DUST_WELL == "000000NA",
        true = "000000.0",
        false = DUST_WELL
      ),
      PAINT = dplyr::if_else(
        PAINT == "000000NA",
        true = "000000.0",
        false = PAINT
      ),
      SOIL = dplyr::if_else(
        SOIL == "000000NA",
        true = "000000.0",
        false = SOIL
      ),
      WATER = dplyr::if_else(
        WATER == "000000NA",
        true = "000000.0",
        false = WATER
      )
    ) %>%
    dplyr::bind_cols(key) %>%
    dplyr::select(
      FILEID,
      tidyselect::all_of(colnames(key)),
      tidyselect::all_of(toupper(vars_inv)),
      DATE_DUE, INV_CLOS_RES, CLEAR_DATE, CLEAR_RSLT
    )

  df_inv
}
