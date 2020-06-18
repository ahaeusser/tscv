
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
  target <- target_vars(fcst)
  value <- value_var(fcst)

  # Prepare test data
  test <- data %>%
    filter(.data$sample == "test") %>%
    rename(actual = !!sym(value))

  # Join test and forecast data
  data <- left_join(
    x = fcst,
    y = test,
    by = c(target, "split", dttm)
  )

  # Calculate forecast errors (actual - fcst)
  errors <- data %>%
    mutate(!!sym(value) := map_dbl(fcst[[value]], `[[`, "mu")) %>%
    as_tsibble() %>%
    mutate(error = (.data$actual - !!sym(value))) %>%
    select(-c(!!sym(value), .data$actual, .data$.mean))

  return(errors)
}
