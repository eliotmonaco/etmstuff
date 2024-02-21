# Test id_distinct_rows()

animals <- c(
  "Castor canadensis", "Nomascus leucogenys", "Didelphis virginiana",
  "Didelphis virginiana", "Didelphis virginiana", "Castor canadensis",
  "Nomascus leucogenys", "Didelphis virginiana", "Nomascus leucogenys",
  "Castor canadensis", "Nomascus leucogenys", "Nomascus leucogenys",
  "Castor canadensis", "Nomascus leucogenys", "Didelphis virginiana",
  "Didelphis virginiana"
)

dates <- as.Date(c(
  "1980-08-01", "1988-01-17", "2019-10-25",
  "1988-01-17", "1980-08-01", "1988-01-17",
  "2019-10-25", "2019-10-25", "1988-01-17",
  "1988-01-17", "2019-10-25", "1980-08-01",
  "1988-01-17", "2019-10-25", "2019-10-25",
  "1988-01-17"
))

numbers <- c(1:16)

row_id <- c(
  "X01", "X02", "X03", "X04", "X05", "X06", "X07", "X03",
  "X02", "X06", "X07", "X08", "X06", "X07", "X03", "X04"
)

df_animals <- tibble::tibble(animals, dates, numbers, row_id)

saveRDS(df_animals, "tests/testthat/data/animals.rds")
