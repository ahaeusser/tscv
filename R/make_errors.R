
#' @title Calculate forecast errors and percentage errors
#'
#' @description Calculate forecast errors and percentage forecast errors for point forecasts.
#'
#' @details
#' \code{make_errors()} compares point forecasts in \code{future_frame} with the
#' observed values in \code{main_frame}. The two data sets are joined by the
#' series identifier and time index specified in \code{context}.
#'
#' The forecast error is calculated as \code{error = actual - point}. The
#' percentage forecast error is calculated as \code{pct_error = (actual -
#' point / point) * 100}.
#'
#' Positive errors indicate that the forecast is below the observed value.
#' Negative errors indicate that the forecast is above the observed value.
#'
#' The returned data contains:
#' \itemize{
#'   \item \code{series_id}: Unique identifier for the time series as specified
#'   in \code{context}.
#'   \item \code{model}: Forecasting model name.
#'   \item \code{split}: Train-test split identifier.
#'   \item \code{horizon}: Forecast horizon.
#'   \item \code{error}: Forecast error.
#'   \item \code{pct_error}: Percentage forecast error.
#' }
#'
#' @param future_frame A \code{tibble} containing the forecasts. It must contain
#'   the columns specified by \code{context}, as well as \code{model},
#'   \code{split}, \code{horizon}, and \code{point}.
#' @param main_frame A \code{tibble} containing the observed values. It must
#'   contain the series identifier, time index, and value column specified by
#'   \code{context}.
#' @param context A named \code{list} with the identifiers for \code{series_id},
#'   \code{value_id}, and \code{index_id}.
#'
#' @return
#' A \code{tibble} containing forecast errors and percentage forecast errors.
#'
#' @family accuracy functions
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tsibble)
#' library(fabletools)
#' library(fable)
#'
#' context <- list(
#'   series_id = "series",
#'   value_id = "value",
#'   index_id = "index"
#' )
#'
#' main_frame <- M4_monthly_data |>
#'   filter(series %in% c("M23100", "M14395"))
#'
#' split_frame <- make_split(
#'   main_frame = main_frame,
#'   context = context,
#'   type = "first",
#'   value = 120,
#'   n_ahead = 18,
#'   n_skip = 17,
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
#'   as_tsibble(
#'     index = index,
#'     key = c(series, split)
#'   )
#'
#' model_frame <- train_frame |>
#'   model(
#'     "SNAIVE" = SNAIVE(value ~ lag("year"))
#'   )
#'
#' fable_frame <- model_frame |>
#'   forecast(h = 18)
#'
#' future_frame <- make_future(
#'   fable = fable_frame,
#'   context = context
#' )
#'
#' error_frame <- make_errors(
#'   future_frame = future_frame,
#'   main_frame = main_frame,
#'   context = context
#' )
#'
#' error_frame

make_errors <- function(future_frame,
                        main_frame,
                        context) {

  series_id <- context[["series_id"]]
  value_id <- context[["value_id"]]
  index_id <- context[["index_id"]]

  # Prepare test data
  main_frame <- rename(
    .data = main_frame,
    actual = !!sym(value_id)
  )

  # Join test data and forecasts and calculate forecast errors and
  # percentage errors
  error_frame <- left_join(
    x = future_frame,
    y = main_frame,
    by = c(series_id, index_id)) |>
    select(c(!!sym(series_id), "model", "split", "horizon", "point", "actual")) |>
    mutate(error = .data$actual - .data$point) |>
    mutate(pct_error = ((.data$actual - .data$point) / .data$actual) * 100) |>
    select(-c(.data$point, .data$actual))

  return(error_frame)
}
