#' Create a CBLS Investigation table
#'
#' @param df A dataframe of investigation records.
#' @param key A dataframe returned by [cbls_table_key()].
#'
#' @return A dataframe formatted as an Investigation table per CBLS guidelines.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
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
      DATE_REF = gsub("-", "", .data$DATE_REF),
      INSP_COMP = dplyr::if_else(
        is.na(.data$INSP_COMP),
        true = strrep(" ", 8),
        false = gsub("-", "", .data$INSP_COMP)
      ),
      ABAT_COMP = dplyr::if_else(
        is.na(.data$ABAT_COMP),
        true = strrep(" ", 8),
        false = gsub("-", "", .data$ABAT_COMP)
      ),
      YEAR = dplyr::if_else(
        is.na(.data$YEAR),
        true = strrep(" ", 4),
        false = as.character(.data$YEAR)
      ),
      OWNERSHIP = substr(.data$OWNERSHIP, 1, 1),
      DWELL_TYPE = substr(.data$DWELL_TYPE, 1, 1),
      PAINT_HAZ = substr(.data$PAINT_HAZ, 1, 1),
      XRF = stringr::str_pad(
        format(round(as.numeric(sub("<", "", .data$XRF)), digits = 1), nsmall = 1, trim = TRUE),
        width = 5, side = "left", pad = "0"
      ),
      DUST_FLOOR = stringr::str_pad(
        format(round(as.numeric(sub("<", "", .data$DUST_FLOOR)), digits = 1), nsmall = 1, trim = TRUE),
        width = 8, side = "left", pad = "0"
      ),
      FLOOR_MSR = dplyr::case_when(
        grepl("ug", .data$FLOOR_MSR) ~ "U",
        grepl("ppm", .data$FLOOR_MSR) ~ "P",
        TRUE ~ " "
      ),
      DUST_SILL = stringr::str_pad(
        format(round(as.numeric(sub("<", "", .data$DUST_SILL)), digits = 1), nsmall = 1, trim = TRUE),
        width = 8, side = "left", pad = "0"
      ),
      SILL_MSR = dplyr::case_when(
        grepl("ug", .data$SILL_MSR) ~ "U",
        grepl("ppm", .data$SILL_MSR) ~ "P",
        TRUE ~ " "
      ),
      DUST_WELL = stringr::str_pad(
        format(round(as.numeric(sub("<", "", .data$DUST_WELL)), digits = 1), nsmall = 1, trim = TRUE),
        width = 8, side = "left", pad = "0"
      ),
      WELL_MSR = dplyr::case_when(
        grepl("ug", .data$WELL_MSR) ~ "U",
        grepl("ppm", .data$WELL_MSR) ~ "P",
        TRUE ~ " "
      ),
      PAINT = stringr::str_pad(
        format(round(as.numeric(sub("<", "", .data$PAINT)), digits = 1), nsmall = 1, trim = TRUE),
        width = 8, side = "left", pad = "0"
      ),
      PAINT_MSR = dplyr::case_when(
        grepl("ug", .data$PAINT_MSR) ~ "U",
        grepl("ppm", .data$PAINT_MSR) ~ "P",
        grepl("mg", .data$PAINT_MSR) ~ "M",
        TRUE ~ " "
      ),
      SOIL = stringr::str_pad(
        format(round(as.numeric(
          stringr::str_squish(
            stringr::str_remove_all(.data$SOIL, regex("<|ppm", ignore_case = TRUE))
          )
        ), digits = 1), nsmall = 1, trim = TRUE),
        width = 8, side = "left", pad = "0"
      ),
      WATER = stringr::str_pad(
        format(round(as.numeric(
          stringr::str_squish(
            stringr::str_remove_all(.data$WATER, regex("<|ppb", ignore_case = TRUE))
          )
        ), digits = 1), nsmall = 1, trim = TRUE),
        width = 8, side = "left", pad = "0"
      ),
      INDHAZ = substr(.data$INDHAZ, 1, 1),
      DATE_DUE = strrep(" ", 8),
      INV_CLOS_RES = strrep(" ", 1),
      CLEAR_DATE = strrep(" ", 8),
      CLEAR_RSLT = strrep(" ", 1)
    ) %>%
    dplyr::mutate(
      XRF = dplyr::if_else(
        .data$XRF == "000NA",
        true = "000.0",
        false = .data$XRF
      ),
      DUST_FLOOR = dplyr::if_else(
        .data$DUST_FLOOR == "000000NA",
        true = "000000.0",
        false = .data$DUST_FLOOR
      ),
      DUST_SILL = dplyr::if_else(
        .data$DUST_SILL == "000000NA",
        true = "000000.0",
        false = .data$DUST_SILL
      ),
      DUST_WELL = dplyr::if_else(
        .data$DUST_WELL == "000000NA",
        true = "000000.0",
        false = .data$DUST_WELL
      ),
      PAINT = dplyr::if_else(
        .data$PAINT == "000000NA",
        true = "000000.0",
        false = .data$PAINT
      ),
      SOIL = dplyr::if_else(
        .data$SOIL == "000000NA",
        true = "000000.0",
        false = .data$SOIL
      ),
      WATER = dplyr::if_else(
        .data$WATER == "000000NA",
        true = "000000.0",
        false = .data$WATER
      )
    ) %>%
    dplyr::bind_cols(key) %>%
    dplyr::select(
      "FILEID",
      tidyselect::all_of(colnames(key)),
      tidyselect::all_of(toupper(vars_inv)),
      "DATE_DUE", "INV_CLOS_RES", "CLEAR_DATE", "CLEAR_RSLT"
    )

  df_inv
}
