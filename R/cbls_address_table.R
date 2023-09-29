#' Create a CBLS Address table
#'
#' @param df A dataframe of records prepared for CBLS submission.
#' @param key A dataframe returned by [cbls_table_key()].
#' @param registry The CBLS Address Registry.
#'
#' @return A dataframe formatted as an Address table per CBLS guidelines.
#' @export
#'
#' @importFrom magrittr %>%
#'
# @examples
#'
cbls_address_table <- function(df, key, registry) {
  var_check(df, var = "address_registry_id")

  if (!all(df$age < 6)) {
    stop("`df$age` must be < 6 for all records", call. = FALSE)
  }

  var_check(key, var = c("ACTION", "QTR", "RPT_YR", "PGMID"))

  df <- df %>%
    dplyr::filter(address_registry_id != "00000000")

  registry <- registry %>%
    dplyr::distinct(address_registry_id, .keep_all = TRUE)

  df_add <- df %>%
    dplyr::select(address_registry_id) %>%
    dplyr::distinct() %>%
    dplyr::left_join(registry, by = "address_registry_id") %>%
    dplyr::select(
      ADDR_ID = address_registry_id,
      CITY = City,
      CNTY_FIPS = CountyFIPS,
      ZIP = PostalCode,
      STATE = State,
      CENSUS = CensusTract
    ) %>%
    dplyr::mutate(
      FILEID = "ADD",
      CITY = substr(
        stringr::str_pad(
          toupper(CITY),
          width = 15,
          side = "right",
          pad = " "
        ), 1, 15
      ),
      CNTY_FIPS = substr(CNTY_FIPS, 3, 5),
      ZIP = stringr::str_pad(
        sub("-", "", ZIP),
        width = 9,
        side = "right",
        pad = " "
      ),
      CENSUS = stringr::str_pad(
        CENSUS,
        width = 7,
        side = "right",
        pad = " "
      )
    )

  # Add `key` and rearrange columns
  df_add <- df_add %>%
    dplyr::bind_cols(key) %>%
    dplyr::select(
      FILEID,
      tidyselect::all_of(colnames(key)),
      tidyselect::everything()
    )

  # RENOVATED (required)
  df_add$RENOVATED <- 9 # Unknown

  # START_REN (not required)
  df_add$START_REN <- strrep(" ", 8)

  # COMP_REN (not required)
  df_add$COMP_REN <- strrep(" ", 8)

  df_add
}
