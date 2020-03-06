
#' @title Check, convert and shape the input data.
#'
#' @description This function checks whether the input data are a valid tsibble or not (regular spaced in time
#'    and ordered). Furthermore, implicit missing values are turned into explicit missing values (existing
#'    missing values are left untouched). If the data are provided in wide format, they are gathered into
#'    long format. For convenience, the index variable is renamed to \code{date_time}, the key variable is
#'    renamed to \code{variable} and the measurement variable is renamed to \code{value} and the columns
#'    \code{type} and \code{target} are added.
#'
#' @param data A valid tsibble, either in long or in wide format.
#'
#' @return data A tsibble in long format with the following columns:
#'    \itemize{
#'       \item{\code{date_time}: The index variable of the tsibble.}
#'       \item{\code{variable}: Character value. The key variable of the tsibble.}
#'       \item{\code{value}: Numeric value. The measurement variable of the tsibble.}
#'       \item{\code{type}: Character value. The type of the observation (i.e. actual, fcst, error, pct_error).}
#'       \item{\code{target}: Logical value, indicating whether the variable is a target variable or not.}
#'       }
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

  # Rename index variable (for convenience)
  data <- data %>%
    rename(date_time = index_var(data))

  # Check whether the data are in wide format and,
  # if necessary, gather to long format
  if (is_empty(key_vars(data)) == TRUE) {
    data <- data %>%
      gather(
        key = "variable",
        value = "value",
        -date_time)
  }

  # Rename key and measurement variable (for convenience)
  data <- data %>%
    rename(variable = key_vars(data)) %>%
    rename(value = measured_vars(data))

  return(data)
}
