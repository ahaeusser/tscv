
#' @title Summary statistics for time series data
#'
#' @description This function calculates several summary statistics for time series data.
#'    The function calculates the following metrics for all key combinations:
#'
#'    \itemize{
#'       \item{\code{start}: Start date (or date-time)}
#'       \item{\code{end}: End date (or date-time)}
#'       \item{\code{n_obs}: Number of observations per time series}
#'       \item{\code{n_missing}: Number of missing values (NAs)}
#'       \item{\code{pct_missing}: Percentage rate of missing values}
#'       \item{\code{n_zeros}: Number of zero values}
#'       \item{\code{pct_zeros}: Percentage rate of zero values}
#'       }
#'
#' @param .data A \code{tibble} in long format containing time series data.
#' @param context A named \code{list} with the identifiers for \code{seried_id}, \code{value_id} and \code{index_id}.
#'
#' @return data A tibble containing the summary statistics.
#' @export

summarise_data <- function(.data, context) {

  series_id <- context[["series_id"]]
  value_id <- context[["value_id"]]
  index_id <- context[["index_id"]]

  data <- .data %>%
    group_by(!!!syms(series_id)) %>%
    summarise(
      start = first(!!sym(index_id)),
      end = last(!!sym(index_id)),
      n_obs = n(),
      n_missing = sum(is.na(!!sym(value_id))),
      pct_missing = round((.data$n_missing / .data$n_obs) * 100, 2),
      n_zeros = sum(!!sym(value_id) == 0, na.rm = TRUE),
      pct_zeros = round((.data$n_zeros / .data$n_obs) * 100, 2)) %>%
    ungroup()

  return(data)
}
