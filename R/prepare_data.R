
#' @title Check and prepare tsibble data
#'
#' @description
#' Check that the input data is a valid regular and ordered \code{tsibble}, fill
#' implicit gaps if requested, and convert wide data to long format.
#'
#' @details
#' \code{check_data()} is a data preparation helper for time series workflows.
#' It performs three tasks:
#' \itemize{
#'   \item checks that \code{data} is a \code{tsibble};
#'   \item checks that the time index is regular and ordered by key and index;
#'   \item optionally turns implicit missing values into explicit missing values
#'   using \code{fill_gaps()}.
#' }
#'
#' If the input data has no key variables, it is treated as wide data and is
#' converted to long format. The resulting output contains the original index
#' column, a \code{variable} column containing the former column names, and a
#' \code{value} column containing the corresponding observations.
#'
#' Existing explicit missing values are not changed.
#'
#' @param data A \code{tsibble} in long or wide format.
#' @param fill_missing Logical value. If \code{TRUE}, implicit missing values
#'   are turned into explicit missing values with \code{fill_gaps()}.
#'
#' @return
#' A \code{tsibble} prepared for downstream use. Wide data is returned in long
#' format with one measurement variable named \code{value}.
#'
#' @family data preparation
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tsibble)
#'
#' data <- M4_monthly_data |>
#'   filter(series %in% c("M23100", "M14395")) |>
#'   as_tsibble(
#'     index = index,
#'     key = series
#'   )
#'
#' check_data(data)
#'
#' wide_data <- data |>
#'   as_tibble() |>
#'   select(index, series, value) |>
#'   tidyr::pivot_wider(
#'     names_from = series,
#'     values_from = value
#'   ) |>
#'   as_tsibble(index = index)
#'
#' check_data(wide_data)

check_data <- function(data,
                       fill_missing = TRUE) {

  # Check input data
  if (is_tsibble(data) == FALSE) {
    stop("Please provide a tsibble (class tbl_ts).")
  }

  if (is_regular(data) == FALSE) {
    stop("The tsibble is not spaced at regular time.")
  }

  if (is_ordered(data) == FALSE) {
    stop("The tsibble is not ordered by key and index.")
  }

  # Turn implicit missing values into explicit missing values
  if (fill_missing == TRUE) {
    data <- data |>
      fill_gaps(.full = TRUE)
  }

  # Check whether the data are in wide format and,
  # if necessary, pivot to long format
  if (is_empty(key_vars(data)) == TRUE) {
    data <- data |>
      pivot_longer(
        cols = -all_of(index_var(data)),
        names_to = "variable",
        values_to = "value"
      )
  }

  return(data)
}



#' @title Identify and replace outliers
#'
#' @description
#' Identify outliers in a numeric time series and replace them with smoothed
#' values.
#'
#' @details
#' \code{smooth_outlier()} is a small wrapper around
#' \code{forecast::tsoutliers()}. The input vector is first converted to an
#' \code{msts} object using the seasonal periods supplied in \code{periods}.
#'
#' For non-seasonal time series, \code{forecast::tsoutliers()} uses a
#' \code{supsmu}-based approach. For seasonal time series, the series is
#' decomposed using STL and outliers are identified on the remainder component.
#' Detected outliers are replaced by the replacement values returned by
#' \code{forecast::tsoutliers()}.
#'
#' The function returns a plain numeric vector with the same length as the input.
#'
#' @param x Numeric vector containing the time series observations.
#' @param periods Numeric vector giving the seasonal periods of the time series,
#'   for example \code{12} for monthly data with yearly seasonality or
#'   \code{c(24, 168)} for hourly data with daily and weekly seasonality.
#' @param ... Further arguments passed to \code{forecast::msts()} or
#'   \code{forecast::tsoutliers()}.
#'
#' @return
#' A numeric vector where detected outliers are replaced by smoothed values.
#'
#' @family data preparation
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' x <- M4_monthly_data |>
#'   filter(series == first(series)) |>
#'   pull(value)
#'
#' x_outlier <- x
#' x_outlier[20] <- x_outlier[20] * 5
#'
#' x_smoothed <- smooth_outlier(
#'   x = x_outlier,
#'   periods = 12
#' )
#'
#' x_outlier[20]
#' x_smoothed[20]
#'
#' hourly <- elec_price |>
#'   filter(bidding_zone == "DE") |>
#'   slice_head(n = 24 * 14) |>
#'   pull(value)
#'
#' hourly_outlier <- hourly
#' hourly_outlier[48] <- hourly_outlier[48] * 5
#'
#' smooth_outlier(
#'   x = hourly_outlier,
#'   periods = c(24, 168)
#' )

smooth_outlier <- function(x,
                           periods,
                           ...) {
  # Create msts object
  x <- msts(data = x, seasonal.periods = periods)

  # Identify outliers
  xs <- forecast::tsoutliers(x = x, ...)

  # Replace outliers
  x[xs$index] <- xs$replacements
  x <- as.numeric(x)

  return(x)
}



#' @title Interpolate missing values
#'
#' @description Interpolate missing values in a numeric time series.
#'
#' @details
#' \code{interpolate_missing()} is a small wrapper around
#' \code{forecast::na.interp()}. The input vector is first converted to an
#' \code{msts} object using the seasonal periods supplied in \code{periods}.
#'
#' For non-seasonal time series, missing values are replaced using linear
#' interpolation. For seasonal time series, \code{forecast::na.interp()} uses an
#' STL-based approach: the series is decomposed, the seasonally adjusted series
#' is interpolated, and the seasonal component is added back.
#'
#' The function returns a plain numeric vector with the same length as the input.
#'
#' @param x Numeric vector containing the time series observations.
#' @param periods Numeric vector giving the seasonal periods of the time series,
#'   for example \code{12} for monthly data with yearly seasonality or
#'   \code{c(24, 168)} for hourly data with daily and weekly seasonality.
#' @param ... Further arguments passed to \code{forecast::msts()} or
#'   \code{forecast::na.interp()}.
#'
#' @return
#' A numeric vector with missing values interpolated.
#'
#' @family data preparation
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' x <- M4_monthly_data |>
#'   filter(series == first(series)) |>
#'   pull(value)
#'
#' x_missing <- x
#' x_missing[c(10, 20, 30)] <- NA
#'
#' x_interpolated <- interpolate_missing(
#'   x = x_missing,
#'   periods = 12
#' )
#'
#' anyNA(x_missing)
#' anyNA(x_interpolated)
#'
#' hourly <- elec_price |>
#'   filter(bidding_zone == "DE") |>
#'   slice_head(n = 24 * 14) |>
#'   pull(value)
#'
#' hourly_missing <- hourly
#' hourly_missing[c(24, 48, 72)] <- NA
#'
#' interpolate_missing(
#'   x = hourly_missing,
#'   periods = c(24, 168)
#' )

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
