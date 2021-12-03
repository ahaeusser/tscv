
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
#' @param .data A \code{tibble} in long format containing time series data.
#' @param context A named \code{list} with the identifiers for \code{seried_id}, \code{value_id} and \code{index_id}.
#'
#' @return data A tibble containing the summary statistics.
#' @export

summarise_stats <- function(.data, context) {

  series_id <- context[["series_id"]]
  value_id <- context[["value_id"]]

  data <- .data %>%
    group_by(!!sym(series_id)) %>%
    summarise(
      mean = mean(!!sym(value_id), na.rm = TRUE),
      median = median(!!sym(value_id), na.rm = TRUE),
      mode = estimate_mode(!!sym(value_id), na_rm = TRUE),
      sd = sd(!!sym(value_id), na.rm = TRUE),
      p0 = quantile(!!sym(value_id), probs = 0, na.rm = TRUE),
      p25 = quantile(!!sym(value_id), probs = 0.25, na.rm = TRUE),
      p75 = quantile(!!sym(value_id), probs = 0.75, na.rm = TRUE),
      p100 = quantile(!!sym(value_id), probs = 1, na.rm = TRUE),
      skewness = estimate_skewness(!!sym(value_id), na_rm = TRUE),
      kurtosis = estimate_kurtosis(!!sym(value_id), na_rm = TRUE)) %>%
    ungroup()

  return(data)
}
