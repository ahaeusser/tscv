
#' @title Summary statistics for time series data
#'
#' @description This function calculates several summary statistics for time series data.
#'    The function calculates the following metrics for all key combinations:
#'
#'    \itemize{
#'       \item{\code{start}: Start date (or date-time)}
#'       \item{\code{end}: End date (or date-time)}
#'       \item{\code{n_missing}: Number of missing values (NAs)}
#'       \item{\code{complete_rate}: Percentage rate of complete values}
#'       \item{\code{mean}: Arithmetic mean}
#'       \item{\code{sd}: Standard deviation}
#'       \item{\code{p0}: Minimum}
#'       \item{\code{p25}: 25\%-Quantile}
#'       \item{\code{p50}: Median}
#'       \item{\code{p75}: 75\%-Quantile}
#'       \item{\code{p100}: Maximum}
#'       \item{\code{skewness}: Skewness}
#'       \item{\code{kurtosis}: Kurtosis}
#'       }
#'
#' @param .data A valid \code{tsibble} in long format with one measurement variable.
#'
#' @return data_tbl A tibble containing the summary statistics.
#' @export

summarise_data <- function(.data) {

  dttm <- index_var(.data)
  response <- response_vars(.data)
  value <- value_var(.data)

  data_tbl <- .data %>%
    as_tibble() %>%
    group_by(!!!syms(response)) %>%
    summarise(
      start = first(!!sym(dttm)),
      end = last(!!sym(dttm)),
      n_obs = n(),
      n_missing = sum(is.na(!!sym(value))),
      complete_rate = sum(!is.na(!!sym(value))) / n(),
      mean = mean(!!sym(value), na.rm = TRUE),
      sd = sd(!!sym(value), na.rm = TRUE),
      p0 = quantile(!!sym(value), probs = 0, na.rm = TRUE),
      p25 = quantile(!!sym(value), probs = 0.25, na.rm = TRUE),
      p50 = quantile(!!sym(value), probs = 0.5, na.rm = TRUE),
      p75 = quantile(!!sym(value), probs = 0.75, na.rm = TRUE),
      p100 = quantile(!!sym(value), probs = 1, na.rm = TRUE),
      skewness = skewness_vec(!!sym(value), na_rm = TRUE),
      kurtosis = kurtosis_vec(!!sym(value), na_rm = TRUE)) %>%
    ungroup()

  return(data_tbl)
}
