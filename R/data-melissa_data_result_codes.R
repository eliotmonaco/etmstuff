#' Melissa Data result codes
#'
#' @name melissa_data_result_codes
#'
#' @description Addresses sent to Melissa Data for validation are returned with result codes in the `Results` column. These codes provide information about the validity of the address and indicate if errors were found or changes were made to the submitted address. `melissa_data_result_codes` is a list containing five tables, each of which is a different result code category:
#'
#' * `address_status`
#' * `address_error`
#' * `address_change`
#' * `geocode_status`
#' * `geocode_error`
#'
#' @usage melissa_data_result_codes
#'
#' @format A list containing five dataframes.
#'
#' @source [Melissa Wiki - Result Code Details](https://wiki.melissadata.com/index.php?title=Result_Code_Details#Personator_Consumer)
#'
#' @keywords datasets
"melissa_data_result_codes"
