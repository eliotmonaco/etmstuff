# Global variable declarations to suppress note in R CMD check, as in "function: no visible binding for global variable `x`"

utils::globalVariables(
  c(
    colnames(utils::data(epitrax_variables)),
    # calculate_age()
    "d1_is_na", "d2_is_na", "age",
    # clean_lab_results()
    "lab_result_clean",
    # clean_values()
    "removed_text", "replacement_text",
    # count_conflicts()
    "n",
    # created by parse_lab_results()
    "lab_result_symbol", "lab_result_number",
    # undupe()
    "dupe_id", "dupe_order",
    # validate_values()
    "n_row"
  )
)
