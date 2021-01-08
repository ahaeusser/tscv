
#' @title Summary statistics for time series data
#'
#' @description This function calculates several summary statistics for time series data.
#'    The function calculates the following metrics for all key combinations:
#'
#'    \itemize{
#'       \item{\code{mean}: Arithmetic mean}
#'       \item{\code{median}: Median (p50)}
#'       \item{\code{mode}: Mode}
#'       \item{\code{sd}: Standard deviation}
#'       \item{\code{p0}: Minimum}
#'       \item{\code{p25}: 25\%-Quantile}
#'       \item{\code{p75}: 75\%-Quantile}
#'       \item{\code{p100}: Maximum}
#'       \item{\code{skewness}: Skewness}
#'       \item{\code{kurtosis}: Kurtosis}
#'       }
#'
#' @param .data A valid \code{tsibble} in long format with one measurement variable.
#'
#' @return data A tibble containing the summary statistics.
#' @export

summarise_stats <- function(.data) {

  keys <- key_vars(.data)
  value <- value_var(.data)

  data <- .data %>%
    as_tibble() %>%
    group_by(!!!syms(keys)) %>%
    summarise(
      mean = mean(!!sym(value), na.rm = TRUE),
      median = median(!!sym(value), na.rm = TRUE),
      mode = estimate_mode(!!sym(value), na_rm = TRUE),
      sd = sd(!!sym(value), na.rm = TRUE),
      p0 = quantile(!!sym(value), probs = 0, na.rm = TRUE),
      p25 = quantile(!!sym(value), probs = 0.25, na.rm = TRUE),
      p75 = quantile(!!sym(value), probs = 0.75, na.rm = TRUE),
      p100 = quantile(!!sym(value), probs = 1, na.rm = TRUE),
      skewness = estimate_skewness(!!sym(value), na_rm = TRUE),
      kurtosis = estimate_kurtosis(!!sym(value), na_rm = TRUE)) %>%
    ungroup()

  return(data)
}
