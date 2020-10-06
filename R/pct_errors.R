
#' @title Calculate percentage forecast errors
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
  pct_errors <- data %>%
    as_tsibble() %>%
    mutate(!!sym(value) := map_dbl(fcst[[value]], `[[`, "mu")) %>%
    mutate(pct_error = ((.data$actual - !!sym(value)) / .data$actual) * 100) %>%
    select(-c(!!sym(value), .data$actual, .data$.mean))

  return(pct_errors)
}
