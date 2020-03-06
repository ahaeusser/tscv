
#' @title Interpolate missing values.
#' 
#' @description This function interpolates missing values. Leading NAs or backcasted, trailing NAs are forecasted
#'    and centered NAs are interpolated. The user can choose between several modeling procedures.
#'
#' @param data A tsibble in long format with standard columns (.date, .variable, .value, etc.) provided by the function \code{shape_data(...)}.
#' @param method Character value. Either \code{arima} or \code{tslm}.
#'
#' @return data A tsibble in long format with standard columns (.date, .variable, .value, etc.)
#' @export

fill_NAs <- function(data,
                     method = "arima") {
  
  # Model the time series (by key ".variable")
  if (method == "arima") {
    models <- data %>%
      model(arima = ARIMA(.value))
  }
  
  if (method == "tslm") {
    models <- data %>%
      model(tslm = TSLM(.value ~ trend()))
  }
  
  # Interpolate missing values by model and arrange by variable
  interpolated <- models %>%
    interpolate(data) %>%
    arrange(.variable)
  
  # Arrange data by variable and replace values
  data <- data %>%
    arrange(.variable) %>%
    mutate(.value = interpolated$.value)
  
  return(data)
}