# Generates `md_summarize` used in `md_results_table()`



codes_list <- list(
  c("AS01", "valid address", "AS01(?!(.{1,50}AS23)|(.{1,50}AS26))"),
  c("AS01 + AS23", "valid address + extraneous street information", "AS01(?=.{1,50}AS23)"),
  c("AS01 + AS26", "valid address + unidentified data", "AS01(?=.{1,50}AS26)"),
  c("AS02", "valid building, missing/invalid unit", "AS02"),
  c("AS03", "exists but not serviced by USPS", "AS03"),
  c("AS13", "converted from rural- to city-style addres by LACS", "AS13"),
  c("AE02", "unknown street", "AE02"),
  c("AE06", "early warning system", "AE06"),
  c("AC02", "state/province added or changed", "AC02"),
  c("AC20", "house number changed", "AC20"),
  c("NA", "no response from Melissa Data", NA)
)

df <- as.data.frame(matrix(nrow = length(codes_list), ncol = 3))

colnames(df) <- c("code", "description", "pattern")

for (i in 1:nrow(df)) {
  df[i,] <- codes_list[[i]]
}

saveRDS(df, "data-raw/md_summarize.rds")
