# Test functions using address tables

df_addr1 <- tibble::tribble(
  ~name, ~street, ~unit, ~city, ~state, ~zip,
  "Curtis Bldg.", "1000 SW Jackson St", "Ste 130", "Topeka", "KS", "66612",
  "Curtis Bldg.", "1000 SW Jackson St", NA, "Topeka", "KS", "66612",
  "Brown v. Board of Ed. Site", "1515 SE Monroe St", NA, "Topeka", "KS", "66612",
  "KU Natural History Museum", "1345 Jayhawk Blvd", NA, "Lawrence", "KS", "66045",
  "K-State Insect Zoo", "1500 Denison Ave", NA, "Manhattan", "KS", "66506",
  "Topeka Zoo", "635 SW Gage Blvd", NA, "Topeka", "KS", "66606",
  "Cosmosphere", "1100 North Plum Street", NA, "Hutchinson", "KS", "67501",
  "Capitol Bldg.", "201 SW 8th", NA, "Topeka", "KS", "66603"
)

df_addr1 <- df_addr1 |>
  id_distinct_rows(id_name = "id", prefix = "AD") |>
  dplyr::relocate(id)

df_addr2 <- tibble::tribble(
  ~name, ~street, ~unit, ~city, ~state, ~zip,
  "Constitution Hall", "429 S Kansas Ave", "#427", "Topeka", "KS", "66603",
  "Kansas City Zoo", "6800 Zoo Dr", NA, "Kansas City", "MO", "64132",
  "KU Natural History Museum", "1345 Jayhawk", NA, "Lawrence", "KS", "66045",
  "Capitol Bldg.", "201 Southwest Eighth Avenue", NA, "Topeka", "KS", "66603",
  "Curtis Bldg.", "1000 Southwest Jackson Street", NA, "Topeka", "KS", "66612",
  "Brown v. Board of Ed. Site", "1515 Southeast Monroe Street", NA, "Topeka", "KS", "66612",
  "K-State Veterinary Diagnostic Lab", "1800 Denison Ave", NA, "Manhattan", "KS", "66506",
  "Curtis Bldg.", "1000 Southwest Jackson Street", "#130", "Topeka", "KS", "66612"
)

df_addr2 <- df_addr2 |>
  id_distinct_rows(id_name = "id", prefix = "AD") |>
  dplyr::relocate(id)

df_match <- fuzzy_compare(
  df_addr1, df_addr2, row_id = "id",
  fuzzy_var = c("street", "unit"),
  exact_var = c("city", "state", "zip")
)

saveRDS(df_addr1, "tests/testthat/data/addresses1.rds")
saveRDS(df_addr2, "tests/testthat/data/addresses2.rds")
saveRDS(df_match, "tests/testthat/data/addresses_fz.rds")
