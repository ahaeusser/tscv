
#' @title Identify and smooth outliers.
#'
#' @description \code{smooth_outliers} identifies and adjusts outliers. The time
#'    series is decomposed via \code{STL} decomposition into trend, season and
#'    remainder. The IQR method is then applied to the remainder to
#'    identify and replace outliers with NAs. Afterwards, the NAs are linearly
#'    interpolated and the time series is additive reconstructed.
#'
#' @param data A \code{tsibble} containing the time series data.
#' @param alpha Numeric value. Controls the width of the limits.
#'
#' @return data A \code{tsibble} containing the outlier adjusted time series.
#'
#' @export

smooth_outlier <- function(data,
                           alpha = 0.05) {

  dttm <- index_var(data)
  keys <- key_vars(data)
  value <- value_var(data)

  smoothed <- data %>%
    model(STL(!!sym(value))) %>%
    components() %>%
    as_tibble() %>%
    select(-c(.data$.model, .data$season_adjust)) %>%
    group_by(!!!syms(keys)) %>%
    mutate(remainder_adjust = approx_vec(iqr_vec(.data$remainder, alpha = alpha))) %>%
    ungroup() %>%
    mutate(!!sym(value) := rowSums(select(., .data$trend, starts_with("season_"), .data$remainder_adjust))) %>%
    as_tsibble(
      index = !!sym(dttm),
      key = c(!!!syms(keys))) %>%
    select(-c(.data$trend, starts_with("season_"), .data$remainder, .data$remainder_adjust))

  data <- data %>%
    mutate(!!sym(value) := smoothed[[as_name(value)]])

  return(data)
}
