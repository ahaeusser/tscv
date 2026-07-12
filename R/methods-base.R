
#' @title Forecast a time series using the Naive2 method
#'
#' @description Produces forecasts using the Naive2 benchmark from the M4 forecasting
#'  competition. The function first applies \code{test_seasonality()} at the
#'  specified frequency. If seasonality is not detected, the final observation
#'  is repeated for the complete forecast horizon.
#'
#'  If seasonality is detected, the series is adjusted using classical
#'  multiplicative decomposition. A naive forecast is then produced from the
#'  seasonally adjusted series and reseasonalized using the estimated seasonal
#'  factors.
#'
#' @param x A numeric vector containing the observed time series.
#' @param freq Integer value. A positive whole number specifying the seasonal
#'  frequency, such as `12` for monthly data or `4` for quarterly data.
#' @param n_ahead Integer value. A positive whole number specifying the number
#'  of future observations to forecast.
#'
#' @return A numeric vector of length `n_ahead` containing the point forecasts.
#'
#' @examples
#' x <- as.numeric(AirPassengers)
#'
#' forecast_naive2(
#'   x = x,
#'   freq = 12,
#'   n_ahead = 18
#' )
#'
#' @export

forecast_naive2 <- function(x, freq, n_ahead) {

  stopifnot(
    is.numeric(x),
    length(x) > 0,
    all(is.finite(x)),
    length(freq) == 1,
    is.finite(freq),
    freq >= 1,
    freq == as.integer(freq),
    length(n_ahead) == 1,
    is.finite(n_ahead),
    n_ahead >= 1,
    n_ahead == as.integer(n_ahead)
  )

  if (!test_seasonality(x, freq)) {
    return(rep(tail(x, 1), n_ahead))
  }

  decomposition <- decompose(
    x = ts(x, frequency = freq),
    type = "multiplicative"
  )

  seasonal_factors <- as.numeric(decomposition$seasonal)
  adjusted_x <- x / seasonal_factors

  future_seasonality <- rep(
    tail(seasonal_factors, freq),
    length.out = n_ahead
  )

  rep(tail(adjusted_x, 1), n_ahead) * future_seasonality
}
