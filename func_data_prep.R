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



