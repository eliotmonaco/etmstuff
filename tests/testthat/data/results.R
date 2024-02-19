# Test lab_results functions

library(dplyr)

## Problem values that `check_results()` will catch
values1 <- c(",< 3.0", ",1", ",3", "< 3.", "< 3..0", "< 3.0.", "< 3.0`", "<2.0 ug/dL", "= 1.1", "= 11.0", "= 5", "=1", "0.4 mcg/dL", "1..0", "11.1 ug/dl", "3.1ug/dl", "4.", "6.2.1", "> 6.2.1", "Mycobacterium avium-intracellulare complex", "none detected", "None detected", "None Detected", "Not detected", "Not Detected", "O157", "Reactive", "Toxin 1 & 2 Positive")

## Valid values
values3 <- c("1.0", "< 1.0", "< 2.0", "< 3.0", "< 3.3", "> 55")

## Problem values that `flag_results()` will catch
values4 <- c("0", "< 0", "< 1.1", "> 2.0", "> 2.1.1", "< 5.0", "101")

df_results <- dplyr::tibble(result = c(values1, values2, values3, values4))

df_results <- df_results %>%
  mutate(
    valid1 = check_results(result),
    clean = clean_results(result)
  ) %>%
  mutate(
    valid2 = check_results(clean),
    result_sign = parse_results(clean)$sign,
    result_number = parse_results(clean)$number
  ) %>%
  mutate(flag_result = flag_results(result_sign, result_number))

saveRDS(df_results, "tests/testthat/data/results.rds")
