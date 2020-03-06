
#' @title Calculate errors and percentage errors.
#'
#' @description This function calculates the errors (actual - fcst) and percentage errors ((actual - fcst) / fcst * 100).
#'
#' @param data A tsibble in long format with standard columns (date_time, variable, etc.).
#'
#' @return errors A tsibble with the following columns:
#'    \itemize{
#'       \item{date_time: Index variable}
#'       \item{variable: Target variable}
#'       \item{sample: Train or test}
#'       \item{slice: Time slice}
#'       \item{horizon: Forecast horizon (n-step ahead)}
#'       \item{type: Error or percentage error}
#'       \item{model: Forecasting model}
#'       \item{value: Measurement variable}
#'       }
#'
#' @export

create_errors <- function(data) {

  # Prepare forecasts
  fcsts <- data %>%
    as_tibble() %>%
    filter(sample == "test") %>%
    filter(type == "fcst") %>%
    arrange(date_time, variable, slice, horizon)

  n_models <- length(unique(fcsts$model))

  # Prepare actual values
  actual <- data %>%
    as_tibble() %>%
    filter(sample == "test") %>%
    filter(type == "actual") %>%
    slice(rep(1:n(), times = n_models)) %>%
    arrange(date_time, variable, slice, horizon)

  # Calculate errors
  error <- fcsts %>%
    mutate(type = "error") %>%
    mutate(value = actual$value - fcsts$value)

  # Calculate percentage errors
  pct_error <- fcsts %>%
    mutate(type = "pct_error") %>%
    mutate(value = ((actual$value - fcsts$value) / actual$value) * 100)

  errors <- bind_rows(
    error,
    pct_error) %>%
    as_tsibble(
      index = date_time,
      key = c(variable, slice, model, type))
}
