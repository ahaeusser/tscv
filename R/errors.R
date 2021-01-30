
#' @title Calculate forecast errors
#'
#' @description \code{error} calculates the forecast errors (actual - fcst).
#'
#' @param fcst A \code{fable} containing the forecasts for the models, splits, etc.
#' @param test A \code{tsibble} containing the testing data.
#'
#' @return errors A \code{tsibble}.
#' @export

errors <- function(fcst,
                   test) {

  dttm <- index_var(fcst)
  target <- target_vars(fcst)
  value <- value_var(fcst)

  # Prepare test data
  test <- rename(
    .data = test,
    actual = !!sym(value))

  # Join test and forecast data
  data <- left_join(
    x = fcst,
    y = test,
    by = c(target, "split", dttm)
  )

  # Calculate forecast errors (actual - fcst)
  errors <- data %>%
    as_tsibble() %>%
    mutate(!!sym(value) := map_dbl(fcst[[value]], `[[`, "mu")) %>%
    mutate(error = (.data$actual - !!sym(value))) %>%
    select(-c(!!sym(value), .data$actual, .data$.mean))

  return(errors)
}
