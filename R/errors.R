
#' @title Calculate forecast errors.
#'
#' @description \code{error} calculates the forecast errors (actual - fcst).
#'
#' @param fcst A \code{fable} containing the forecasts for the models, splits, etc.
#' @param data A \code{tsibble} containing the training and testing data.
#'
#' @return errors A \code{tsibble}.
#' @export

errors <- function(fcst,
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
  errors <- data %>%
    mutate(error = (actual - !!sym(value))) %>%
    select(-c(!!sym(value), actual, .distribution))

  return(errors)
}
