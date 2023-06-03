## open_source_zip() ####

open_source_zip <- function(zip_path, filename, save=T, ...) {

  dots <- list(...)

  lng <- nchar(filename)
  lst <- stringr::str_locate_all(filename, "\\.")
  point <- tail(lst[[1]], 1)[1]

  filex <- substr(filename, point + 1, lng)

  if (toupper(filex) == "CSV") {
    df <- readr::read_csv(unz(zip_path, filename),
                          col_types = readr::cols(.default = "c"),
                          na = c("", "NA", "NULL"))
  }

  df

}



## validate_var_names() ####

# Same as test_vector_equality() but specific to dataframe variable names
# (uses colnames() on `df`).

validate_var_names <- function(df, ref, ignore_case=F) {

  if (ignore_case) {

    df_in_ref <- all(toupper(colnames(df)) %in% toupper(ref))
    ref_in_df <- all(toupper(ref) %in% toupper(colnames(df)))

    if (df_in_ref == F) {
      df_missing <- colnames(df)[which(!toupper(colnames(df)) %in% toupper(ref))]
      message("Variables in the dataframe that are not present in the reference list:\n  ",
              sprintf(paste(df_missing, collapse = ", ")))
    } else {
      message("All variables in the dataframe are present in the reference list.")
    }

    if (ref_in_df == F) {
      ref_missing <- ref[which(!toupper(ref) %in% toupper(colnames(df)))]
      message("Variables in the reference list that are not present in the dataframe:\n  ",
              sprintf(paste(ref_missing, collapse = ", ")))
    } else {
      message("All variables in the reference list are present in the dataframe.")
    }

  } else {

    df_in_ref <- all(colnames(df) %in% ref)
    ref_in_df <- all(ref %in% colnames(df))

    if (df_in_ref == F) {
      df_missing <- colnames(df)[which(!colnames(df) %in% ref)]
      message("Variables in the dataframe that are not present in the reference list:\n  ",
              sprintf(paste(df_missing, collapse = ", ")))
    } else {
      message("All variables in the dataframe are present in the reference list.")
    }

    if (ref_in_df == F) {
      ref_missing <- ref[which(!ref %in% colnames(df))]
      message("Variables in the reference list that are not present in the dataframe:\n  ",
              sprintf(paste(ref_missing, collapse = ", ")))
    } else {
      message("All variables in the reference list are present in the dataframe.")
    }

  }

}



## check_expected_values() ####

# Searches for expected values in the variables used for deduplication. If
# an unexpected value is found, it is returned in the output DF.

check_expected_values <- function(df) {

  df_val <- data.frame("recno" = df$recno,
                       "patient_id" = stringr::str_detect(df$patient_id, "^\\d{3,7}$"),
                       "patient_record_number" = stringr::str_detect(df$patient_record_number, "^\\d{10,12}$"),
                       "patient_birth_date" = stringr::str_detect(df$patient_birth_date, "^\\d{4}-\\d{2}-\\d{2}"),
                       "lab_result_value" = stringr::str_detect(df$lab_result_value, regex("(\\d{1,3})|(NOT|NONE DETECTED)", ignore_case = T)),
                       "lab_collection_date" = stringr::str_detect(df$lab_collection_date, "^\\d{4}-\\d{2}-\\d{2}"))

  df_val$all_valid <- apply(df_val[, 2:5], 1, all)

  df_val <- df_val[which(!Vectorize(isTRUE)(df_val$all_valid)),]

  if (nrow(df_val) == 0) {
    return(message("The expected values for critical variables are present in all records"))
  } else {
    df_val <- df_val[-which(colnames(df_val) == "all_valid")]
  }

  f <- function(df_val, df_orig) {
    df_val2 <- as.data.frame(apply(df_val, 2, as.character))
    pbmax <- nrow(df_val)
    pb <- txtProgressBar(1, pbmax, width = 50, style = 3)
    for (i in 1:nrow(df_val)) {
      for (j in 2:ncol(df_val)) {
        if (df_val[i, j] == F | is.na(df_val[i, j])) {
          id <- df_val$recno[i]
          row <- which(df_orig$recno == id)
          col <- colnames(df_val)[j]
          val <- df_orig[[row, col]]
          df_val2[i, j] <- val
        }
      }
      setTxtProgressBar(pb, i)
    }
    df_val2
  }

  close(pb)

  f(df_val, df)

}



## check_date_seq() ####

# Evaluates whether or not the date values in date_vars are in sequence
# (i.e., in the order listed in the argument).

check_date_seq <- function(df, id, date_vars) {

  suppressWarnings({
    library(dplyr)
  })

  df <- df %>%
    select({{ id }}, {{ date_vars }})

  f <- function(r) {
    v <- as.Date(r)
    v <- as.numeric(v)
    v <- na.omit(v)
    all(v == cummax(v))
  }

  df$dates_in_seq <- apply(df %>% select({{ date_vars }}), 1, f)

  df <- df[which(!Vectorize(isTRUE)(df$dates_in_seq)),]

  if (nrow(df) == 0) {
    message("The date values are in sequence in all records.")
  } else {
    df
  }

}



## check_suff_addr() ####

# Determines if values are present in a combination of address variables
# that is sufficient for geocoding an address.

check_suff_addr <- function(df) {

  suppressWarnings({
    library(dplyr)
  })

  df1 <- df["recno"]

  for (i in 1:2) {

    if (i == 1) {
      addr <- c("lab_collection_street", "lab_collection_city", "lab_collection_postal_code")
    } else {
      addr <- c("current_address_street", "current_address_city", "current_address_zip")
    }

    df2 <- df %>%
      select(recno, {{ addr }}) %>%
      standardize()

    f <- function(r) {
      # r[addr[1]] != "" & (r[addr[2]] != "" | r[addr[3]] != "")
      !is.na(r[addr[1]]) & (!is.na(r[addr[2]]) | !is.na(r[addr[3]]))
    }

    nm <- paste0("addr", i)

    df2[nm] <- apply(df2, 1, f)

    df1 <- df1 %>%
      left_join(df2, by = "recno")

  }

  df1 <- df1[which(!Vectorize(isTRUE)(df1$addr1) & !Vectorize(isTRUE)(df1$addr2)),]

  if (nrow(df1) == 0) {
    message("Values are present for a sufficient address for all records.")
  } else {
    df1
  }

}



## aggregate_values_loop() ####

# In a dataframe of dupe sets, values within each variable in `var` will be
# concatenated if they belong to rows with the same `id` value. Values are
# separated by `sep`, with NAs and blank character strings omitted. The output
# is a dataframe of aggregated `var` values matched to the corresponding `id`
# value (so one row per unique `id` value).

aggregate_values_loop <- function(df, var, id, sep=" | ") {

  source("2_functions/func_general.R")

  var_check(df, var = c(var, id))

  suppressWarnings({
    library(dplyr)
  })

  # List of unique `id` values
  id_unq <- unique(df[[id]])

  # DF to hold aggregated `var` values
  n_rows <- length(id_unq)
  n_cols <- length(var)
  df_agg <- data.frame(cbind(id_unq, matrix(nrow = n_rows, ncol = n_cols)))
  colnames(df_agg) <- c(id, var)

  pbmax <- n_rows * n_cols
  pb <- txtProgressBar(1, pbmax, width = 50, style = 3)

  for (i in 1:n_cols) { # Loop through variables in `var`
    for (j in 1:n_rows) { # Use each value in `id_unq` to aggregate `var` values
      df_set <- df %>%
        filter(.data[[id]] == id_unq[j])
      values <- na.omit(unique(df_set[[var[i]]]))
      if (is.character(values)) values <- values[values != ""]
      # browser()
      df_agg[[var[i]]][j] <- paste0(values, collapse = sep)
      setTxtProgressBar(pb, (i - 1) * n_rows + j)
    }
  }

  close(pb)

  df_agg

}



## dive_into_DF() ####

dive_into_DF <- function(df, vars, id, start_row=1) {

  # if (!all(vars %in% colnames(df))) {
  #   stop("Make sure the variables chosen are present in the dataframe.")
  # }

  source("2_functions/func_general.R")

  var_check(df, var = vars)

  suppressWarnings({
    library(svDialogs)
    library(stringr)
  })

  i <- start_row

  while (i <= nrow(df)) {

    string <- tidyr::unite(df[i,][vars], col = "x", sep = "; ")[1, 1]

    resp_edit <- dlg_input(paste0("Make changes here to ", id, " ", df[[id]][i], ". Click Cancel to skip."),
                           default = string)$res

    if (!purrr::is_empty(resp_edit)) {
      input <- str_trim(unlist(strsplit(resp_edit, ";")))
      df[i,][vars] <- input
    }

    resp_id <- dlg_input(paste("Enter a new", id, "to jump to that record,",
                               "or click Cancel to edit the next one.",
                               "Enter \"q\" to quit."))$res
    resp_id <- toString(resp_id)

    if (resp_id == "q") {
      break
    } else if (resp_id != "" & !(resp_id %in% df[[id]])) {
      resp_id <- dlg_input(paste("Enter a new", id, "to jump to that record,",
                                 "or click Cancel to edit the next one.",
                                 "Enter \"q\" to quit."))$res
      resp_id <- toString(resp_id)
    }

    if (resp_id != "") {
      i = which(df[[id]] == resp_id)
    } else {
      i <- i + 1
    }

  }

  df

}







