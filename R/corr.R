


#' @title Estimate the sample autocorrelation of a numeric vector
#'
#' @description \code{acf_vec} estimates the sample autocorrelation function
#'   of a numeric vector.
#'
#' @param x Numeric vector.
#' @param lag_max Maximum lag as integer.
#' @param ... Further arguments passed to \code{stats::acf()}.
#'
#' @return x Numeric vector.
#' @export

acf_vec <- function(x, lag_max = 24, ...) {
  # Estimate autocorrelation function and drop lag 0
  x <- as.numeric(acf(x = x,  lag.max = lag_max, plot = FALSE, ...)$acf)
  x <- tail(x, -1)
  return(x)
}


#' @title Estimate the sample partial autocorrelation of a numeric vector
#'
#' @description \code{acf_vec} estimates the sample partial autocorrelation
#'   function of a numeric vector.
#'
#' @param x Numeric vector.
#' @param lag_max Maximum lag as integer.
#' @param ... Further arguments passed to \code{stats::acf()}.
#'
#' @return x Numeric vector.
#' @export

pacf_vec <- function(x, lag_max = 24, ...) {
  # Estimate autocorrelation function
  x <- as.numeric(pacf(x = x,  lag.max = lag_max, plot = FALSE, ...)$acf)
  return(x)
}


#' @title Estimate the sample autocorrelation
#'
#' @description The function estimates the sample autocorrelation function of a
#'   \code{tibble} containing several time series.
#'
#' @param .data A \code{tibble} containing the time series data.
#' @param context A named \code{list} with the identifiers for \code{seried_id}, \code{value_id} and \code{index_id}.
#' @param lag_max Maximum lag as integer.
#' @param ... Further arguments passed to \code{stats::acf()}.
#'
#' @return data A \code{tibble} with a column for the unique identifier of the
#'   time series and the columns type, lag and value.
#' @export

estimate_acf <- function(.data,
                         context,
                         lag_max = 24,
                         ...) {

  series_id <- context[["series_id"]]
  value_id <- context[["value_id"]]

  data <- .data %>%
    select(!!sym(series_id), !!sym(value_id)) %>%
    group_by(!!sym(series_id)) %>%
    summarise(
      type = "ACF",
      lag = 1:lag_max,
      value = acf_vec(x = !!sym(value_id), lag_max = lag_max, ...),
      .groups = "drop"
    )

  return(data)
}


#' @title Estimate the sample partial autocorrelation
#'
#' @description The function estimates the sample partial autocorrelation
#'   function of a \code{tibble} containing several time series.
#'
#' @param .data A \code{tibble} containing the time series data.
#' @param context A named \code{list} with the identifiers for \code{seried_id}, \code{value_id} and \code{index_id}.
#' @param lag_max Maximum lag as integer.
#' @param ... Further arguments passed to \code{stats::pacf()}.
#'
#' @return data A \code{tibble} with a column for the unique identifier of the
#'   time series and the columns type, lag and value.
#' @export

estimate_pacf <- function(.data,
                          context,
                          lag_max = 24,
                          ...) {

  series_id <- context[["series_id"]]
  value_id <- context[["value_id"]]

  data <- .data %>%
    select(!!sym(series_id), !!sym(value_id)) %>%
    group_by(!!sym(series_id)) %>%
    summarise(
      type = "PACF",
      lag = 1:lag_max,
      value = pacf_vec(x = !!sym(value_id), lag_max = lag_max, ...),
      .groups = "drop"
    )

  return(data)
}
