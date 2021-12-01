
#' @title Identify and replace outliers
#'
#' @description The function \code{smooth_outlier()} is a wrapper
#'   for \code{forecast::tsoutliers()}, working with numeric vectors. For
#'   non-seasonal time series, the supsmu method is used. For seasonal
#'   time series, the series is decomposed via STL and the IQR method is
#'   used on the remainder component. Values outside the range are
#'   linear interpolated on the remainder and the series is reconstructed
#'   with the corrected remainder component.
#'
#' @param x Numeric vector.
#' @param period Numeric vector. The seasonal periods of the time series.
#' @param ... Further arguments passed to \code{forecast::msts()} or \code{forecast::tsoutliers()}.
#'
#' @return Numeric vector.
#' @export

smooth_outlier <- function(x,
                           period,
                           ...) {
  # Create msts object
  x <- msts(data = x, seasonal.periods = period)

  # Identify outliers
  xs <- forecast::tsoutliers(x = x, ...)

  # Replace outliers
  x[xs$index] <- xs$replacements
  x <- as.numeric(x)

  return(x)
}
