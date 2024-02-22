# Test id_distinct_rows(), undupe(), dupeset_conflicts

name <- c(
  "Castor canadensis", "Nomascus leucogenys", "Didelphis virginiana",
  "Didelphis virginiana", "Didelphis virginiana", "Castor canadensis",
  "Nomascus leucogenys", "Didelphis virginiana", "Nomascus leucogenys",
  "Castor canadensis", "Nomascus leucogenys", "Nomascus leucogenys",
  "Castor canadensis", "Nomascus leucogenys", "Didelphis virginiana",
  "Didelphis virginiana"
)

date <- as.Date(c(
  "1980-08-01", "1988-01-17", "2019-10-25",
  "1988-01-17", "1980-08-01", "1988-01-17",
  "2019-10-25", "2019-10-25", "1988-01-17",
  "1988-01-17", "2019-10-25", "1980-08-01",
  "1988-01-17", "2019-10-25", "2019-10-25",
  "1988-01-17"
))

number <- c(1:16)

row_id <- c(
  "X01", "X02", "X03", "X04", "X05", "X06", "X07", "X03",
  "X02", "X06", "X07", "X08", "X06", "X07", "X03", "X04"
)

df <- tibble::tibble(name, date, number, row_id)

undp <- undupe(df, var = c("name", "date"), prefix = "anml")

df_count <- count_conflicts(undp$dupesets, dupe_id = "anml_id")

df_isolate <- isolate_conflicts(undp$dupesets, var = c("number", "row_id"), dupe_id = "anml_id")

df_flat <- flatten_conflicts(undp$dupesets, dupe_id = "anml_id")

saveRDS(df, "tests/testthat/data/animals.rds")
saveRDS(undp, "tests/testthat/data/animals_undp.rds")
saveRDS(df_count, "tests/testthat/data/animals_count.rds")
saveRDS(df_isolate, "tests/testthat/data/animals_isolate.rds")
saveRDS(df_flat, "tests/testthat/data/animals_flat.rds")
