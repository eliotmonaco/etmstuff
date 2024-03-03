# Generates `test_addresses.rda`

test_addresses <- tibble::tribble(
  ~name, ~street, ~unit, ~city, ~state, ~zip,
  "Curtis Bldg.", "1000 SW Jackson St", "Ste 130", "Topeka", "KS", "66612",
  "Constitution Hall", "429 S Kansas Ave", "427", "Topeka", "KS", "66603",
  "Brown v. Board of Ed. Site", "1515 SE Monroe St", NA, "Topeka", "KS", "66612",
  "Capitol Bldg.", "201 Southwest Eighth Avenue", NA, "Topeka", "KS", "66603",
  "Capitol Bldg.", "201 SW 8th", NA, "Topeka", "KS", "66603",
  "Topeka Zoo", "635 SW Gage Blvd", NA, "Topeka", "KS", "66606",
  "Kansas City Zoo", "6800 Zoo Dr", NA, "Kansas City", "MO", "64132",
  "KU Natural History Museum", "1345 Jayhawk Blvd", NA, "Lawrence", "KS", "66045",
  "K-State Insect Zoo", "1500 Denison Ave", NA, "Manhattan", "KS", "66506",
  "K-State Veterinary Diagnostic Lab", "1800 Denison Ave", NA, "Manhattan", "KS", "66506",
  "Cosmosphere", "1100 North Plum Street", NA, "Hutchinson", "KS", "67501"
)

test_addresses <- test_addresses |>
  id_distinct_rows(id_name = "address_id", prefix = "AD") |>
  dplyr::relocate(address_id)

usethis::use_data(test_addresses, overwrite = T)
