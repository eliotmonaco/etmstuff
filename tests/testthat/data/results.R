# Test lab_results functions

library(dplyr)

## Problem values that `check_results()` will catch
values1 <- c(",< 3.0", ",1", ",3", "< 3.", "< 3..0", "< 3.0.", "< 3.0`", "<2.0 ug/dL", "= 1.1", "= 11.0", "= 5", "=1", "0.4 mcg/dL", "1..0", "11.1 ug/dl", "3.1ug/dl", "4.", "6.2.1", "> 6.2.1", "Mycobacterium avium-intracellulare complex", "none detected", "None detected", "None Detected", "Not detected", "Not Detected", "O157", "Reactive", "Toxin 1 & 2 Positive")

## Problem values that `check_results()` won't catch
values2 <- c("> 2.0", "> 2.1.1", "< 5.0")

## Valid values
values3 <- c("1.0", "> 55", "< 1.0")

df_results <- dplyr::tibble(result = c(values1, values2, values3))

df_results <- df_results %>%
  mutate(
    valid1 = etmstuff::check_results(result),
    clean = etmstuff::clean_results(result)
  ) %>%
  mutate(
    valid2 = etmstuff::check_results(clean),
    result_sign = etmstuff::parse_results(clean)$sign,
    result_number = etmstuff::parse_results(clean)$number
  )

saveRDS(df_results, "tests/testthat/data/results.rds")
