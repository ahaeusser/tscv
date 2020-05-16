
#' @title Forecast univariate models.
#'
#' @description This function takes a model object of class \code{mdl_df} and produces forecasts for a given
#'    forecast horizon.
#'
#' @param models A model object of class \code{mdl_df} ("mable"). The result of a call to \code{fabletools::model(...)}.
#' @param n_ahead Integer value. The forecast horizon (n-step-ahead).
#'
#' @return A forecast object of class \code{fbl_ts} ("fable") containing the forecasts for all variables, slices and models.
#' @export

forecast_model <- function(models,
                           n_ahead) {

  date_time <- index_var(data)
  variable <- key_vars(data)
  value <- data %>%
    select(
      -c(sample,
         horizon,
         type,
         model)) %>%
    measured_vars()

  fcsts <- models %>%
    fabletools::forecast(h = n_ahead) %>%
    group_by_key() %>%
    mutate(horizon = row_number()) %>%
    mutate(sample = "test") %>%
    mutate(type = "fcst") %>%
    ungroup() %>%
    as_tsibble(index = !!sym(date_time)) %>%
    # rename(distribution = .distribution) %>%
    rename(model = .model) %>%
    select(
      !!sym(date_time),
      !!!syms(variable),
      sample,
      slice,
      horizon,
      type,
      model,
      !!sym(value))

  return(fcsts)
}
