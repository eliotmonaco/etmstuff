#' Assign test reason to lead test records
#'
#' @description
#' This function assigns a reason to each test in `df` based on the KDHE Elevated Blood Lead Disease Investigation Guidelines. Each test in a sequence should be within a maximum of 92 days of the prior test, therefore `max_interval = 92` is the default. The reason assigned to each test depends on values in one or more prior tests, therefore it is recommended that a dataframe of records containing a `test_reason` variable from the 92-day period immediately prior to the data in `df` be provided in `df_past`. It is also recommended that only values of `Blood - capillary` or `Blood - venous` be allowed in `lab_specimen_source`, as any other value will lead to a reason of `unknown` in subsequent tests.
#'
#' `test_reason` values:
#'
#' * `cap_scrn`: A capillary screening test. This must be the first test in a sequence.
#' * `ven_cfm_init`: An initial venous confirmatory test. This must be the first test in a sequence.
#' * `cap_cfm_elev`: A capillary confirmatory test following an elevated `cap_scrn`.
#' * `cap_cfm_nonelev`: A capillary confirmatory test following a non-elevated `cap_scrn`.
#' * `ven_cfm_elev`: A venous confirmatory test following an elevated `cap_scrn`.
#' * `ven_cfm_nonelev`: A venous confirmatory test following a non-elevated `cap_scrn`.
#' * `ven_flw`: A venous follow-up test. This is any venous test following either an elevated venous test or an elevated capillary confirmatory test, i.e., `cap_cfm_elev` or `cap_cfm_nonelev`.
#' * `unknown`: A test that cannot be assigned another value.
#'
#' @param df A dataframe of lead test records.
#' @param max_interval The maximum number of days between tests belonging to the same test sequence (an integer). The default value is `92`.
#' @param df_past A dataframe of records immediately preceding the records in `df` (optional).
#' @param silent Logical: silence progress indicator if `TRUE`.
#'
#' @return A dataframe with the new variables `test_reason` and `test_seq_alert`.
#' @export
#'
#' @importFrom magrittr %>%
#'
# @examples
#'
assign_test_reason <- function(df, max_interval = 92, df_past = NULL, silent = FALSE) {
  vars <- c(
    "dupe_id", "patient_id", "lab_collection_date",
    "lab_result_number", "lab_result_elev", "lab_specimen_source"
  )
  vars_new <- c("test_reason", "test_seq_alert")

  var_check(df, var = vars)

  # Prep `df_target`
  df_target <- df %>%
    dplyr::select(tidyselect::all_of(vars)) %>%
    dplyr::arrange(patient_id, lab_collection_date, lab_specimen_source) %>%
    dplyr::mutate(
      test_reason = NA,
      test_seq_alert = NA
    )

  # Prep `df_past`
  if (!is.null(df_past)) {
    var_check(df_past, var = c(vars, vars_new))
    df_past <- df_past %>%
      dplyr::select(tidyselect::all_of(c(vars, vars_new))) %>%
      dplyr::arrange(patient_id, lab_collection_date, lab_specimen_source)
  }

  # Empty row for when no prior test is found (used in `get_prior_test()`)
  empty_test <- tibble::as_tibble(
    as.data.frame.list(
      matrix(nrow = 1, ncol = 8),
      col.names = colnames(df_target)
    )
  )

  # prior_test_list <- list()

  if (!silent) pb <- utils::txtProgressBar(1, nrow(df_target), width = 50, style = 3)

  for (i in 1:nrow(df_target)) {
    # Get prior test
    # prior_test_list[[i]] <- get_prior_test(
    prior_test <- get_prior_test(
      df1 = df_target,
      df2 = df_past,
      row = i,
      days = max_interval,
      blank = empty_test
    )

    # Populate `test_reason` and `test_seq_alert`
    df_target[i, vars_new] <- df_target[i,] %>%
      dplyr::mutate(
        test_reason = fn_reason(
          cur_src = lab_specimen_source, prior_src = prior_test$lab_specimen_source,
          prior_rsn = prior_test$test_reason, prior_elev = prior_test$lab_result_elev
        ),
        test_seq_alert = fn_seq_alert(
          cur_src = lab_specimen_source, prior_src = prior_test$lab_specimen_source,
          prior_rsn = prior_test$test_reason, prior_elev = prior_test$lab_result_elev
        )
      ) %>%
      dplyr::select(tidyselect::all_of(vars_new))

    if (!silent) utils::setTxtProgressBar(pb, i)
  }

  if (!silent) close(pb)

  # df_prior <- purrr::list_rbind(prior_test_list)
  #
  # df_target <- df_target %>%
  #   dplyr::bind_cols(
  #     df_prior %>%
  #       dplyr::rename_with(~ paste0(.x, "2"), tidyselect::everything())
  #   )

  # for (i in 1:nrow(df_target)) {
  #   df_target[i, vars_new] <- df_target[i,] %>%
  #     dplyr::mutate(
  #       test_reason = fn_reason(
  #         cur_src = lab_specimen_source, prior_src = lab_specimen_source2,
  #         prior_rsn = test_reason2, prior_elev = lab_result_elev2
  #       ),
  #       test_seq_alert = fn_seq_alert(
  #         cur_src = lab_specimen_source, prior_src = lab_specimen_source2,
  #         prior_rsn = test_reason2, prior_elev = lab_result_elev2
  #       )
  #     ) %>%
  #     dplyr::select(tidyselect::all_of(vars_new))
  # }

  # Join `test_reason` and `test_seq_alert` back to `df`
  df %>%
    dplyr::left_join(
      df_target %>%
        dplyr::select(dupe_id, tidyselect::all_of(vars_new)),
      by = "dupe_id"
    )
}

get_all_tests <- function(df1, df2 = NULL, row) {
  df1 %>%
    dplyr::bind_rows(df2) %>%
    dplyr::filter(patient_id == df1$patient_id[row])
}

get_prior_test <- function(df1, df2 = NULL, row, days, blank) {
  # Get all tests from prior interval for one person (same-day tests included)
  past_tests <- df1 %>%
    dplyr::bind_rows(df2) %>%
    dplyr::filter(
      patient_id == df1$patient_id[row],
      lab_collection_date <= df1$lab_collection_date[row],
      lab_collection_date >= df1$lab_collection_date[row] - days,
      dupe_id != df1$dupe_id[row]
    )

  # Get all tests from most recent test date
  prior_test <- past_tests %>%
    dplyr::slice_max(lab_collection_date, n = 1, with_ties = TRUE)

  if (nrow(prior_test) > 1) {
    # If multiple tests are found on the prior test date...
    if ("Blood - venous" %in% prior_test$lab_specimen_source) {
      # If any venous are present, keep only venous, then keep the highest result
      prior_test <- prior_test %>%
        dplyr::filter(lab_specimen_source == "Blood - venous") %>%
        dplyr::slice_max(lab_result_number, n = 1, with_ties = FALSE)
    } else {
      # If only capillary are present, keep the highest result
      prior_test <- prior_test %>%
        dplyr::slice_max(lab_result_number, n = 1, with_ties = FALSE)
    }
  }

  if (nrow(prior_test) == 0) {
    # If no prior test is found, fill the prior test with NAs
    prior_test <- blank
  } else {
    # If `prior_test` contains only one test...
    if (prior_test$lab_collection_date == df1$lab_collection_date[row] &
        (prior_test$lab_specimen_source != "Blood - capillary" | df1$lab_specimen_source[row] != "Blood - venous")) {
      # If the test is same-day, the prior test must be capillary and the current test must be venous, otherwise look on an earlier day
      prior_test <- past_tests %>%
        dplyr::filter(lab_collection_date < df1$lab_collection_date[row]) %>%
        dplyr::slice_max(lab_collection_date, n = 1, with_ties = TRUE)
      if (nrow(prior_test) == 0) {
        # If no other prior test is found, fill the prior test with NAs
        prior_test <- blank
      } else if (nrow(prior_test) > 1) {
        # Same reduction logic as above
        if ("Blood - venous" %in% prior_test$lab_specimen_source) {
          prior_test <- prior_test %>%
            dplyr::filter(lab_specimen_source == "Blood - venous") %>%
            dplyr::slice_max(lab_result_number, n = 1, with_ties = FALSE)
        } else {
          prior_test <- prior_test %>%
            dplyr::slice_max(lab_result_number, n = 1, with_ties = FALSE)
        }
      }
    }
  }

  prior_test
}

fn_reason <- function(cur_src, prior_src, prior_rsn, prior_elev) {
  dplyr::case_when(
    cur_src == "Blood - venous" & (is.na(prior_rsn) | (prior_src == "Blood - venous" & !prior_elev)) ~
      "ven_cfm_init", # src = ven; prior rsn = NA OR (prior src = ven AND prior BLL = non-elevated)
    cur_src == "Blood - capillary" & (is.na(prior_rsn) | (prior_src == "Blood - venous" & !prior_elev)) ~
      "cap_scrn", # src = cap; prior rsn = NA OR (prior src = ven AND prior BLL = non-elevated)
    cur_src == "Blood - venous" & prior_elev &
      (prior_src == "Blood - venous" | (prior_rsn == "cap_cfm_elev" | prior_rsn == "cap_cfm_nonelev")) ~
      "ven_flw", # src = ven; prior BLL = elevated; prior src = ven OR prior rsn = cap_cfm_elev|cap_cfm_nonelev
    cur_src == "Blood - venous" & prior_rsn == "cap_scrn" & prior_elev ~
      "ven_cfm_elev", # src = ven; prior rsn = cap_scrn; prior BLL = elevated
    cur_src == "Blood - capillary" & prior_rsn == "cap_scrn" & prior_elev ~
      "cap_cfm_elev", # src = cap; prior rsn = cap_scrn; prior BLL = elevated
    cur_src == "Blood - venous" & prior_rsn == "cap_scrn" & !prior_elev ~
      "ven_cfm_nonelev", # src = ven; prior rsn = cap_scrn; prior BLL = non-elevated
    cur_src == "Blood - capillary" & prior_rsn == "cap_scrn" & !prior_elev ~
      "cap_cfm_nonelev", # src = cap; prior rsn = cap_scrn; prior BLL = non-elevated
    TRUE ~ "unknown" # All else
  )
}

fn_seq_alert <- function(cur_src, prior_src, prior_rsn, prior_elev) {
  dplyr::case_when(
    prior_src == "Blood - venous" & !prior_elev ~
      "follows_nonelevated_venous",
    cur_src == "Blood - capillary" & prior_rsn != "cap_scrn" & prior_elev ~
      "follows_elevated_non-cap-scrn",
    TRUE ~ NA
  )
}
