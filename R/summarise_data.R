
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
#'       }
#'
#' @param .data A valid \code{tsibble} in long format with one measurement variable.
#'
#' @return data A tibble containing the summary statistics.
#' @export

summarise_data <- function(.data) {

  dttm <- index_var(.data)
  keys <- key_vars(.data)
  value <- value_var(.data)

  data <- .data %>%
    as_tibble() %>%
    group_by(!!!syms(keys)) %>%
    summarise(
      start = first(!!sym(dttm)),
      end = last(!!sym(dttm)),
      n_obs = n(),
      n_missing = sum(is.na(!!sym(value))),
      complete_rate = round((1 - (n_missing / n_obs)) * 100, 2)) %>%
    ungroup()

  return(data)
}
