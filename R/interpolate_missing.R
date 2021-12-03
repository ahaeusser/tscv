
#' @title Interpolate missing values
#'
#' @description The function \code{interpolate_missing()} is a wrapper
#'   for \code{forecast::na.interp()}, working with numeric vectors. For
#'   non-seasonal time series, linear interpolation is used and for
#'   seasonal time series, the series is decomposed via STL and the
#'   seasonally adjusted series is linearly interpolated and the seasonal
#'   component is added back.
#'
#' @param x Numeric vector.
#' @param periods Numeric vector. The seasonal periods of the time series.
#' @param ... Further arguments passed to \code{forecast::msts()} or \code{forecast::na.interp()}.
#'
#' @return Numeric vector.
#' @export

interpolate_missing <- function(x,
                                periods,
                                ...) {
  # Create msts object
  x <- msts(data = x, seasonal.periods = periods)

  # Interpolate missing values
  x <- forecast::na.interp(x = x, ...)
  x <- as.numeric(x)

  return(x)
}
