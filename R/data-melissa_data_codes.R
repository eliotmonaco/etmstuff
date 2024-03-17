#' Melissa Data result codes
#'
#' @name melissa_data_codes
#'
#' @description Addresses sent to Melissa Data for validation are returned with result codes in the `Results` column. These codes provide information about the validity of the submitted address and indicate if any errors were found or if any changes were made in the response. `melissa_data_codes` is a list of five tables:
#'
#' * `address_status`
#' * `address_error`
#' * `address_change`
#' * `geocode_status`
#' * `geocode_error`
#'
#' @usage melissa_data_codes
#'
#' @format A list of five dataframes.
#'
#' @source Copied on 2024-02-01 from the [Melissa Wiki web page](https://wiki.melissadata.com/index.php?title=Result_Code_Details#Personator_Consumer).
#'
#' @keywords datasets
"melissa_data_codes"
