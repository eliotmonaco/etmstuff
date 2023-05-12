# Same as validate_var_names() but generalized to all vectors.

test_vector_equality <- function(v1, v2, ignore_case=F) {

  if (ignore_case) {

    v1_in_v2 <- all(toupper(v1) %in% toupper(v2))
    v2_in_v1 <- all(toupper(v2) %in% toupper(v1))

    if (v1_in_v2 == F) {
      v1_missing <- v1[which(!toupper(v1) %in% toupper(v2))]
      message("Elements in v1 that are not present in v2:\n  ",
              sprintf(paste(v1_missing, collapse = ", ")))
    } else {
      message("All elements in v1 are present in v2.")
    }

    if (v2_in_v1 == F) {
      v2_missing <- v2[which(!toupper(v2) %in% toupper(v1))]
      message("Elements in v2 that are not present in v1:\n  ",
              sprintf(paste(v2_missing, collapse = ", ")))
    } else {
      message("All elements in v2 are present in v1.")
    }

  } else {

    v1_in_v2 <- all(v1 %in% v2)
    v2_in_v1 <- all(v2 %in% v1)

    if (v1_in_v2 == F) {
      v1_missing <- v1[which(!v1 %in% v2)]
      message("Elements in v1 that are not present in v2:\n  ",
              sprintf(paste(v1_missing, collapse = ", ")))
    } else {
      message("All elements in v1 are present in v2.")
    }

    if (v2_in_v1 == F) {
      v2_missing <- v2[which(!v2 %in% v1)]
      message("Elements in v2 that are not present in v1:\n  ",
              sprintf(paste(v2_missing, collapse = ", ")))
    } else {
      message("All elements in v2 are present in v1.")
    }

  }

}



