
#' @title Check, convert and shape the input data.
#'
#' @description This function checks whether the input data are a valid tsibble or not (regular spaced in time
#'    and ordered). Furthermore, implicit missing values are turned into explicit missing values (existing
#'    missing values are left untouched). If the data are provided in wide format, they are gathered into
#'    long format.
#'
#' @param data A valid tsibble, either in long or in wide format.
#'
#' @return data A valid tsibble in long format with one measurement variable.
#' @export

clean_data <- function(data) {

  # Check input data
  if (is_tsibble(data) == FALSE) {
    stop("Please provide a tsibble (class tbl_ts).")
  }

  if (is_regular(data) == FALSE) {
    stop("The tsibble is not spaced at regular time.")
  }

  if (is_ordered(data) == FALSE) {
    stop("The tsibble is not ordered by key and index.")
  }

  # Turn implicit missing values into explicit missing values
  data <- data %>%
    fill_gaps(.full = TRUE)

  # Check whether the data are in wide format and,
  # if necessary, gather to long format
  if (is_empty(key_vars(data)) == TRUE) {
    data <- data %>%
      gather(
        key = "variable",
        value = "value",
        -!!sym(index_var(data)))
  }

  return(data)
}
