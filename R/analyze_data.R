
#' @title Estimate the mode of a distribution
#'
#' @description
#' Estimate the mode of a numeric distribution using kernel density estimation.
#'
#' @details
#' The function computes a kernel density estimate with \code{stats::density()}
#' and returns the value of \code{x} at which the estimated density is largest.
#'
#' Missing values are removed by default. Additional arguments are passed to
#' \code{stats::density()}, for example \code{bw}, \code{kernel}, or \code{n}.
#'
#' @param x Numeric vector.
#' @param na_rm Logical value. If \code{TRUE}, missing values are removed before
#'   estimation.
#' @param ... Further arguments passed to \code{stats::density()}.
#'
#' @return
#' A numeric value giving the estimated mode of the distribution.
#'
#' @family data analysis
#' @export
#'
#' @examples
#' x <- c(1, 1, 2, 2, 2, 3, 4, NA)
#'
#' estimate_mode(x)
#' estimate_mode(x, na_rm = TRUE)
#' estimate_mode(x, bw = "nrd0")
#'
#' set.seed(123)
#' y <- rnorm(100, mean = 5)
#' estimate_mode(y)

estimate_mode <- function(x,
                          na_rm = TRUE,
                          ...) {

  if (na_rm == TRUE) {
    x <- x[!is.na(x)]
  }

  object <- density(x = x, ...)
  mode_id <- which.max(object$y)
  mode <- object$x[mode_id]

  return(mode)
}



#' @title Estimate kurtosis
#'
#' @description Estimate the kurtosis of a numeric distribution.
#'
#' @details The function computes the moment-based kurtosis
#'
#' \deqn{
#'   \frac{n \sum_i (x_i - \bar{x})^4}
#'        {\left(\sum_i (x_i - \bar{x})^2\right)^2}
#' }
#'
#' Missing values are removed by default.
#'
#' This returns the usual kurtosis, not excess kurtosis. A normal distribution
#' has kurtosis close to \code{3}.
#'
#' @param x Numeric vector.
#' @param na_rm Logical value. If \code{TRUE}, missing values are removed before
#'   estimation.
#'
#' @return A numeric value giving the estimated kurtosis.
#'
#' @family data analysis
#' @export
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5, NA)
#'
#' estimate_kurtosis(x)
#' estimate_kurtosis(x, na_rm = TRUE)
#'
#' set.seed(123)
#' y <- rnorm(100)
#' estimate_kurtosis(y)

estimate_kurtosis <- function(x, na_rm = TRUE) {

  if (na_rm == TRUE) {
    x <- x[!is.na(x)]
  }

  n <- length(x)
  n * sum((x - mean(x))^4) / (sum((x - mean(x))^2)^2)
}



#' @title Estimate skewness
#'
#' @description Estimate the skewness of a numeric distribution.
#'
#' @details
#' The function computes the moment-based skewness
#'
#' \deqn{
#'   \frac{\frac{1}{n}\sum_i (x_i - \bar{x})^3}
#'        {\left(\frac{1}{n}\sum_i (x_i - \bar{x})^2\right)^{3/2}}
#' }
#'
#' Missing values are removed by default. Positive values indicate a distribution
#' with a longer or heavier right tail; negative values indicate a distribution
#' with a longer or heavier left tail.
#'
#' @param x Numeric vector.
#' @param na_rm Logical value. If \code{TRUE}, missing values are removed before
#'   estimation.
#'
#' @return A numeric value giving the estimated skewness.
#'
#' @family data analysis
#' @export
#'
#' @examples
#' x <- c(1, 2, 3, 4, 10, NA)
#'
#' estimate_skewness(x)
#' estimate_skewness(x, na_rm = TRUE)
#'
#' set.seed(123)
#' y <- rexp(100)
#' estimate_skewness(y)

estimate_skewness <- function(x, na_rm = TRUE) {

  if (na_rm == TRUE) {
    x <- x[!is.na(x)]
  }

  n <- length(x)
  (sum((x - mean(x))^3) / n) / (sum((x - mean(x))^2) / n)^(3/2)
}


#' @title Estimate autocorrelations of a numeric vector
#'
#' @description
#' Estimate the sample autocorrelation function of a numeric vector.
#'
#' @details
#' \code{acf_vec()} is a small wrapper around \code{stats::acf()}. It returns
#' the sample autocorrelations as a numeric vector and removes lag 0 from the
#' output, because lag 0 is always equal to 1 and is usually not needed for
#' diagnostics.
#'
#' @param x Numeric vector.
#' @param lag_max Integer. Maximum lag for which the autocorrelation is
#'   estimated.
#' @param ... Further arguments passed to \code{stats::acf()}.
#'
#' @return
#' A numeric vector containing the sample autocorrelations for lags
#' \code{1} to \code{lag_max}.
#'
#' @family data analysis
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' x <- M4_monthly_data |>
#'   filter(series == first(series)) |>
#'   pull(value)
#'
#' acf_vec(
#'   x = x,
#'   lag_max = 12
#' )

acf_vec <- function(x, lag_max = 24, ...) {
  # Estimate autocorrelation function and drop lag 0
  x <- as.numeric(acf(x = x, lag.max = lag_max, plot = FALSE, ...)$acf)
  x <- tail(x, -1)

  return(x)
}



#' @title Estimate partial autocorrelations of a numeric vector
#'
#' @description
#' Estimate the sample partial autocorrelation function of a numeric vector.
#'
#' @details
#' \code{pacf_vec()} is a small wrapper around \code{stats::pacf()}. It returns
#' the sample partial autocorrelations as a numeric vector for lags
#' \code{1} to \code{lag_max}.
#'
#' @param x Numeric vector.
#' @param lag_max Integer. Maximum lag for which the partial autocorrelation is
#'   estimated.
#' @param ... Further arguments passed to \code{stats::pacf()}.
#'
#' @return
#' A numeric vector containing the sample partial autocorrelations for lags
#' \code{1} to \code{lag_max}.
#'
#' @family data analysis
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' x <- M4_monthly_data |>
#'   filter(series == first(series)) |>
#'   pull(value)
#'
#' pacf_vec(
#'   x = x,
#'   lag_max = 12
#' )

pacf_vec <- function(x, lag_max = 24, ...) {
  # Estimate partial autocorrelation function
  x <- as.numeric(pacf(x = x, lag.max = lag_max, plot = FALSE, ...)$acf)

  return(x)
}



#' @title Estimate autocorrelations by time series
#'
#' @description
#' Estimate the sample autocorrelation function for one or more time series in a
#' \code{tibble}.
#'
#' @details
#' \code{estimate_acf()} groups the input data by the series identifier supplied
#' in \code{context} and estimates the sample autocorrelation function for each
#' time series separately.
#'
#' The output contains one row per series and lag. The column \code{bound}
#' contains an approximate significance threshold based on the selected
#' confidence level. The logical column \code{sign} indicates whether the
#' absolute autocorrelation is larger than this threshold.
#'
#' @param .data A \code{tibble} containing the time series data.
#' @param context A named \code{list} with the identifiers for
#'   \code{series_id}, \code{value_id}, and \code{index_id}.
#' @param lag_max Integer. Maximum lag for which the autocorrelation is
#'   estimated.
#' @param level Numeric value. Confidence level used to calculate the
#'   approximate significance bound.
#' @param ... Further arguments passed to \code{stats::acf()}.
#'
#' @return
#' A \code{tibble} with the series identifier and the columns \code{type},
#' \code{lag}, \code{value}, \code{bound}, and \code{sign}.
#'
#' @family data analysis
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' context <- list(
#'   series_id = "series",
#'   value_id = "value",
#'   index_id = "index"
#' )
#'
#' data <- M4_monthly_data |>
#'   filter(series %in% c("M23100", "M14395"))
#'
#' estimate_acf(
#'   .data = data,
#'   context = context,
#'   lag_max = 12
#' )

estimate_acf <- function(.data,
                         context,
                         lag_max = 24,
                         level = 0.9,
                         ...) {

  series_id <- context[["series_id"]]
  value_id <- context[["value_id"]]

  data <- .data |>
    select(all_of(c(series_id, value_id))) |>
    group_by(!!sym(series_id)) |>
    reframe(
      type = "ACF",
      lag = seq_len(lag_max),
      value = acf_vec(x = !!sym(value_id), lag_max = lag_max, ...),
      bound = abs(qnorm((1 - level) / 2) / sqrt(n()))
    ) |>
    mutate(sign = abs(.data$value) > .data$bound)

  return(data)
}



#' @title Estimate partial autocorrelations by time series
#'
#' @description
#' Estimate the sample partial autocorrelation function for one or more time
#' series in a \code{tibble}.
#'
#' @details
#' \code{estimate_pacf()} groups the input data by the series identifier
#' supplied in \code{context} and estimates the sample partial autocorrelation
#' function for each time series separately.
#'
#' The output contains one row per series and lag. The column \code{bound}
#' contains an approximate significance threshold based on the selected
#' confidence level. The logical column \code{sign} indicates whether the
#' absolute partial autocorrelation is larger than this threshold.
#'
#' @param .data A \code{tibble} containing the time series data.
#' @param context A named \code{list} with the identifiers for
#'   \code{series_id}, \code{value_id}, and \code{index_id}.
#' @param lag_max Integer. Maximum lag for which the partial autocorrelation is
#'   estimated.
#' @param level Numeric value. Confidence level used to calculate the
#'   approximate significance bound.
#' @param ... Further arguments passed to \code{stats::pacf()}.
#'
#' @return
#' A \code{tibble} with the series identifier and the columns \code{type},
#' \code{lag}, \code{value}, \code{bound}, and \code{sign}.
#'
#' @family data analysis
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' context <- list(
#'   series_id = "series",
#'   value_id = "value",
#'   index_id = "index"
#' )
#'
#' data <- M4_monthly_data |>
#'   filter(series %in% c("M23100", "M14395"))
#'
#' estimate_pacf(
#'   .data = data,
#'   context = context,
#'   lag_max = 12
#' )

estimate_pacf <- function(.data,
                          context,
                          lag_max = 24,
                          level = 0.9,
                          ...) {

  series_id <- context[["series_id"]]
  value_id <- context[["value_id"]]

  data <- .data |>
    select(all_of(c(series_id, value_id))) |>
    group_by(!!sym(series_id)) |>
    reframe(
      type = "PACF",
      lag = seq_len(lag_max),
      value = pacf_vec(x = !!sym(value_id), lag_max = lag_max, ...),
      bound = abs(qnorm((1 - level) / 2) / sqrt(n()))
    ) |>
    mutate(sign = abs(.data$value) > .data$bound)

  return(data)
}


#' @title Summarise time series data
#'
#' @description
#' Calculate basic data-quality summary statistics for one or more time series.
#'
#' @details
#' \code{summarise_data()} groups the input data by the series identifier
#' supplied in \code{context} and returns one row per time series.
#'
#' The function reports:
#' \itemize{
#'   \item \code{start}: first time index;
#'   \item \code{end}: last time index;
#'   \item \code{n_obs}: number of observations;
#'   \item \code{n_missing}: number of missing values;
#'   \item \code{pct_missing}: percentage of missing values;
#'   \item \code{n_zeros}: number of zero values;
#'   \item \code{pct_zeros}: percentage of zero values.
#' }
#'
#' @param .data A \code{tibble} in long format containing time series data.
#' @param context A named \code{list} with the identifiers for
#'   \code{series_id}, \code{value_id}, and \code{index_id}.
#'
#' @return
#' A \code{tibble} containing one row per time series and the calculated summary
#' statistics.
#'
#' @family data analysis
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' context <- list(
#'   series_id = "series",
#'   value_id = "value",
#'   index_id = "index"
#' )
#'
#' data <- M4_monthly_data |>
#'   filter(series %in% c("M23100", "M14395"))
#'
#' summarise_data(
#'   .data = data,
#'   context = context
#' )

summarise_data <- function(.data, context) {

  series_id <- context[["series_id"]]
  value_id <- context[["value_id"]]
  index_id <- context[["index_id"]]

  data <- .data |>
    group_by(across(all_of(series_id))) |>
    summarise(
      start = first(!!sym(index_id)),
      end = last(!!sym(index_id)),
      n_obs = n(),
      n_missing = sum(is.na(!!sym(value_id))),
      n_zeros = sum(!!sym(value_id) == 0, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      pct_missing = round((.data$n_missing / .data$n_obs) * 100, 2),
      pct_zeros = round((.data$n_zeros / .data$n_obs) * 100, 2)
    ) |>
    select(
      all_of(c(
        series_id,
        "start",
        "end",
        "n_obs",
        "n_missing",
        "pct_missing",
        "n_zeros",
        "pct_zeros"
      ))
    )

  return(data)
}



#' @title Summarise train-test splits
#'
#' @description
#' Summarise the time and row-index ranges of training and test samples.
#'
#' @details
#' \code{summarise_split()} is intended for data sets that contain sliced
#' training and test observations from a time series cross-validation workflow.
#' The input must be a \code{tsibble} with the columns \code{split},
#' \code{sample}, and \code{id}:
#' \itemize{
#'   \item \code{split}: train-test split identifier;
#'   \item \code{sample}: sample label, usually \code{"train"} or \code{"test"};
#'   \item \code{id}: integer row index within the original time series.
#' }
#'
#' The function returns one row per split. For each split, it reports the time
#' range and index range of each sample.
#'
#' @param data A valid \code{tsibble} in long format. It must contain the
#'   columns \code{split}, \code{sample}, and \code{id}.
#'
#' @return
#' A \code{tibble} containing the summarized split ranges.
#'
#' @family data analysis
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tsibble)
#'
#' context <- list(
#'   series_id = "bidding_zone",
#'   value_id = "value",
#'   index_id = "time"
#' )
#'
#' main_frame <- elec_price |>
#'   filter(bidding_zone == "DE") |>
#'   slice_head(n = 120)
#'
#' split_frame <- make_split(
#'   main_frame = main_frame,
#'   context = context,
#'   type = "first",
#'   value = 48,
#'   n_ahead = 24,
#'   n_skip = 23,
#'   n_lag = 0,
#'   mode = "stretch",
#'   exceed = FALSE
#' )
#'
#' train_frame <- slice_train(
#'   main_frame = main_frame,
#'   split_frame = split_frame,
#'   context = context
#' ) |>
#'   mutate(sample = "train")
#'
#' test_frame <- slice_test(
#'   main_frame = main_frame,
#'   split_frame = split_frame,
#'   context = context
#' ) |>
#'   mutate(sample = "test")
#'
#' split_data <- bind_rows(train_frame, test_frame) |>
#'   group_by(bidding_zone, split, sample) |>
#'   mutate(id = row_number()) |>
#'   ungroup() |>
#'   as_tsibble(
#'     index = time,
#'     key = c(bidding_zone, split, sample)
#'   )
#'
#' summarise_split(split_data)

summarise_split <- function(data) {

  date_time <- index_var(data)

  split_tbl <- data |>
    as_tibble() |>
    select(all_of(c("split", "sample", "id", date_time))) |>
    distinct() |>
    group_by(.data$split, .data$sample) |>
    summarise(
      time_start = first(!!sym(date_time)),
      index_start = first(.data$id),
      time_end = last(!!sym(date_time)),
      index_end = last(.data$id),
      .groups = "drop"
    ) |>
    arrange(.data$split, .data$index_start, .data$index_end)

  split_tbl <- split_tbl |>
    mutate(
      time = paste0("[", .data$time_start, ", ", .data$time_end, "]")
    ) |>
    mutate(
      index = paste0(
        "[",
        formatC(
          .data$index_start,
          width = nchar(max(c(.data$index_start, .data$index_end))),
          flag = "0"
        ),
        ", ",
        formatC(
          .data$index_end,
          width = nchar(max(c(.data$index_start, .data$index_end))),
          flag = "0"
        ),
        "]"
      )
    ) |>
    select(
      -all_of(c("time_start", "index_start", "time_end", "index_end"))
    ) |>
    pivot_wider(
      names_from = all_of("sample"),
      values_from = all_of(c("time", "index"))
    )

  return(split_tbl)
}



#' @title Summarise distributional statistics by time series
#'
#' @description
#' Calculate descriptive statistics for one or more time series.
#'
#' @details
#' \code{summarise_stats()} groups the input data by the series identifier
#' supplied in \code{context} and returns one row per time series.
#'
#' The function reports:
#' \itemize{
#'   \item \code{mean}: arithmetic mean;
#'   \item \code{median}: median;
#'   \item \code{mode}: kernel-density based mode estimate;
#'   \item \code{sd}: standard deviation;
#'   \item \code{p0}: minimum;
#'   \item \code{p25}: 25 percent quantile;
#'   \item \code{p75}: 75 percent quantile;
#'   \item \code{p100}: maximum;
#'   \item \code{skewness}: moment-based skewness;
#'   \item \code{kurtosis}: moment-based kurtosis.
#' }
#'
#' Missing values are removed when calculating the statistics.
#'
#' @param .data A \code{tibble} in long format containing time series data.
#' @param context A named \code{list} with the identifiers for
#'   \code{series_id}, \code{value_id}, and \code{index_id}.
#'
#' @return
#' A \code{tibble} containing one row per time series and the calculated
#' descriptive statistics.
#'
#' @family data analysis
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' context <- list(
#'   series_id = "series",
#'   value_id = "value",
#'   index_id = "index"
#' )
#'
#' data <- M4_monthly_data |>
#'   filter(series %in% c("M23100", "M14395"))
#'
#' summarise_stats(
#'   .data = data,
#'   context = context
#' )

summarise_stats <- function(.data, context) {

  series_id <- context[["series_id"]]
  value_id <- context[["value_id"]]

  data <- .data |>
    group_by(across(all_of(series_id))) |>
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
      kurtosis = estimate_kurtosis(!!sym(value_id), na_rm = TRUE),
      .groups = "drop"
    )

  return(data)
}

