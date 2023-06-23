cbls_investigation_table <- function(df, key) {
  
  # etmstuff::var_check(df, var = c("row_id_src", "record_id_src", "patient_id"))
  
  var_check(key, var = c("ACTION", "QTR", "RPT_YR", "PGMID"))
  
  # if (!all(df$age < 6)) stop("The dataframe should contain only records for children under 6 years old", call. = F)
  
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



