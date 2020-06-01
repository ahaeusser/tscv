
#' @title Calculate percentage forecast errors.
#'
#' @description \code{error} calculates percentage forecast errors (actual - fcst / actual) * 100.
#'
#' @param fcst A \code{fable} containing the forecasts for the models, splits, etc.
#' @param data A \code{tsibble} containing the training and testing data.
#'
#' @return errors A \code{tsibble}.
#' @export

pct_errors <- function(fcst,
                       data) {

  dttm <- index_var(fcst)
  response <- response_vars(fcst)
  value <- value_var(fcst)

  # Prepare test data
  test <- data %>%
    filter(sample == "test") %>%
    rename(actual = !!sym(value))

  # Join test and forecast data
  data <- left_join(
    x = fcst,
    y = test,
    by = c(response, "split", dttm)
  )

  # Calculate forecast errors (actual - fcst)
  pct_errors <- data %>%
    mutate(pct_error = ((actual - !!sym(value)) / actual) * 100) %>%
    select(-c(!!sym(value), actual, .distribution))

  return(pct_errors)
}
