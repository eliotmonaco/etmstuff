# Test subset_dates()

dates <- seq.Date(as.Date("2020-01-01"), as.Date("2022-12-31"), by = "day")

dfx <- tibble::tibble(date = dates)

saveRDS(dfx, "tests/testthat/data/dates1.rds")

dfy <- tibble::tibble(
  date = dates,
  number = as.numeric(dates),
  character = as.character(dates)
)

saveRDS(dfy, "tests/testthat/data/dates2.rds")
