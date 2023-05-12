# Assign a sequential ID number to a unique set of variables (`id_vars`).
# `key` should be a unique ID for each row of the input DF that allows
# rejoining after deduplication by `id`.

assign_seq_id <- function(df, id, id_vars, key) {

  suppressWarnings({
    library(dplyr)
  })

  source("2_functions/func_general.R")

  df_std <- standardize(df, replace = "", uppercase = T, vars_ignore = key)

  df_std <- df_std %>%
    select({{ id_vars }}) %>%
    distinct() %>%
    mutate({{ id }} := formatC(x = 1:nrow(.), width = nchar(nrow(df)), flag = "0")) %>%
    right_join(df_std, by = id_vars, multiple = "all")

  df %>%
    left_join(df_std %>%
                select({{ key }}, {{ id }}),
              by = key) %>%
    select({{ key }}, {{ id }}, everything())

}



