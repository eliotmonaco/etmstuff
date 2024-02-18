# Test lab_results functions

values1 <- c(",< 3.0", ",1", ",3", "< 3.", "< 3..0", "< 3.0.", "< 3.0`", "<2.0 ug/dL", "= 1.1", "= 11.0", "= 5", "=1", "0.4 mcg/dL", "1..0", "11.1 ug/dl", "3.1ug/dl", "4.", "6.2.1", "> 6.2.1", "Mycobacterium avium-intracellulare complex", "none detected", "None detected", "None Detected", "Not detected", "Not Detected", "O157", "Reactive", "Toxin 1 & 2 Positive")

values2 <- c("> 2.0", "> 2.1.1", "< 5.0")

values3 <- c("1.0", "> 55", "< 1.0")

df_results <- data.frame(result = c(values1, values2, values3))




df_results$result_valid <- check_test_results(df_results$result)

df_results$result_clean <- clean_test_results(df_results$result)

df_results$result_valid2 <- check_test_results(df_results$result_clean)

parsed_results <- parse_test_results(df_results$result_clean)
df_results$result_sign <- parsed_results$sign
df_results$result_number <- parsed_results$number
