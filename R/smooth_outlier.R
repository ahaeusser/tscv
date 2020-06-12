
#' @title Identidy and smooth outliers.
#'
#' @description \code{smooth_outliers} identifies and adjusts outliers. The time
#'    series is decomposed via \code{STL} decomposition into trend, season and
#'    remainder. The IQR method is then applied to the remainder to
#'    identidy and replace outliers with NAs. Afterwards, the NAs are linearly
#'    interpolated and the time series is additively reconstructed.
#'
#' @param data A \code{tsibble} containing the time series data.
#' @param alpha Numeric value. Controls the width of the limits.
#'
#' @return data A \code{tsibble} containing the outlier adjusted time series.

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
    select(-c(.model, season_adjust)) %>%
    group_by(!!!syms(keys)) %>%
    mutate(remainder_adjust = approx_vec(iqr_vec(remainder, alpha = alpha))) %>%
    ungroup() %>%
    mutate(!!sym(value) := rowSums(select(., trend, starts_with("season_"), remainder_adjust))) %>%
    as_tsibble(
      index = !!sym(dttm),
      key = c(!!!syms(keys))) %>%
    select(-c(trend, starts_with("season_"), remainder, remainder_adjust))

  data <- data %>%
    mutate(!!sym(value) := smoothed[[as_name(value)]])

  return(data)
}
