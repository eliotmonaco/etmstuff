# Test test_results functions

## Problem values that `check_results()` will catch
values1 <- c(",< 3.0", ",1", ",3", "< 3.", "< 3..0", "< 3.0.", "< 3.0`", "<2.0 ug/dL", "= 1.1", "= 11.0", "= 5", "=1", "0.4 mcg/dL", "1..0", "11.1 ug/dl", "3.1ug/dl", "4.", "6.2.1", "> 6.2.1", "Mycobacterium avium-intracellulare complex", "none detected", "None detected", "None Detected", "Not detected", "Not Detected", "O157", "Reactive", "Toxin 1 & 2 Positive")

## Valid values
values2 <- c("1.0", "< 1.0", "< 2.0", "< 3.0", "< 3.3", "> 55")

## Problem values that `flag_results()` will catch
values3 <- c("0", "< 0", "< 1.1", "> 2.0", "> 2.1.1", "< 5.0", "101")

df_results <- tibble::tibble(result = c(values1, values2, values3))

df_results <- df_results |>
  dplyr::mutate(
    valid1 = check_results(result),
    clean = clean_results(result)
  ) |>
  dplyr::mutate(
    valid2 = check_results(clean),
    result_sign = parse_results(clean)$sign,
    result_number = parse_results(clean)$number
  ) |>
  dplyr::mutate(flag_result = flag_results(result_sign, result_number))

saveRDS(df_results, "tests/testthat/data/results.rds")
