
#' @title Summary statistics for time series data.
#'
#' @description This function calculates several summary statistics for time series data.
#'    The function calculates the following statistics for all key combinations:
#'
#'    \itemize{
#'       \item{\code{n_missing}: Number of missing values (NAs)}
#'       \item{\code{complete_rate}: Percentange rate of complete values}
#'       \item{\code{mean}: Mean}
#'       \item{\code{sd}: Standard Deviation}
#'       \item{\code{p0}: Minimum}
#'       \item{\code{p25}: 25\%-Quantile}
#'       \item{\code{p50}: Median}
#'       \item{\code{p75}: 75\%-Quantile}
#'       \item{\code{p100}: Maximum}
#'       \item{\code{hist}: Histogram as sparkline}
#'       }
#'
#' @param data A valid tsibble in long format with one measurement variable.
#'
#' @return summary_tbl A tibble containing the summary statistics.
#' @export

summarise_data <- function(data) {

  date_time <- index_var(data)
  variable <- key_vars(data)
  value <- measured_vars(data)

  if (length(value) > 1) {
    abort("Only one measured variable is supported.")
  }

  data <- data %>%
    as_tibble() %>%
    select(-!!sym(date_time)) %>%
    group_by(!!!syms(variable))

  skim_adj <- skim_with(
    numeric = sfl(n_obs = length))

  summary_tbl <- skim_adj(data = data) %>%
    as_tibble() %>%
    select(-c(skim_type, skim_variable)) %>%
    rename(
      mean = numeric.mean,
      sd = numeric.sd,
      p0 = numeric.p0,
      p25 = numeric.p25,
      p50 = numeric.p50,
      p75 = numeric.p75,
      p100 = numeric.p100,
      hist = numeric.hist,
      n_obs = numeric.n_obs)

  summary_tbl <- summary_tbl %>%
    select(
      c(!!!syms(variable)),
      n_obs,
      n_missing,
      complete_rate,
      mean,
      sd,
      p0,
      p25,
      p50,
      p75,
      p100,
      hist)

  return(summary_tbl)
}
