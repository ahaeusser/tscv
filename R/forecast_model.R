
#' @title Forecast univariate models.
#'
#' @description This function takes a model object of class \code{mdl_df} and produces forecasts for a given
#'    forecast horizon.
#'
#' @param models A model object of class \code{mdl_df} ("mable"). The result of a call to \code{model_univariate(...)}.
#' @param n_ahead Integer value. The forecast horizon (n-step-ahead).
#'
#' @return A forecast object of class \code{fbl_ts} ("fable") containing the forecasts for all target variables, slices and the defined models.
#' @export

forecast_model <- function(models,
                           n_ahead) {

  fcsts <- models %>%
    fabletools::forecast(h = n_ahead) %>%
    group_by(slice, variable, .model) %>%
    mutate(horizon = row_number()) %>%
    mutate(sample = "test") %>%
    mutate(type = "fcst") %>%
    ungroup() %>%
    as_tsibble(index = date_time) %>%
    select(-.distribution) %>%
    rename(model = .model) %>%
    select(
      date_time,
      variable,
      sample,
      slice,
      horizon,
      type,
      model,
      value)

  return(fcsts)
}
