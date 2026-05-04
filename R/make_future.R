
#' @title Convert forecasts to a future frame
#'
#' @description
#' Convert forecasts from a \code{fable} object to a standardized forecast
#' table.
#'
#' @details
#' \code{make_future()} converts the output of \code{forecast()} into a
#' \code{tibble} with a consistent structure for downstream evaluation,
#' plotting, and accuracy calculation.
#'
#' The returned \code{future_frame} contains one row per forecasted observation,
#' time series, split, and model. It includes the following columns:
#' \itemize{
#'   \item the time index column specified by \code{context$index_id};
#'   \item the series identifier column specified by \code{context$series_id};
#'   \item \code{model}: the forecasting model name;
#'   \item \code{split}: the train-test split identifier;
#'   \item \code{horizon}: the forecast horizon within each series, split, and
#'   model;
#'   \item \code{point}: the point forecast, taken from the \code{.mean} column
#'   of the \code{fable}.
#' }
#'
#' This format is used by functions such as \code{make_accuracy()} and
#' \code{make_errors()}.
#'
#' @param fable A \code{fable} created with \code{forecast()}.
#' @param context A named \code{list} with the identifiers for
#'   \code{series_id}, \code{value_id}, and \code{index_id}.
#'
#' @return
#' A \code{tibble} containing forecasts in standardized \code{future_frame}
#' format.
#'
#' @family time series cross-validation
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tsibble)
#' library(fable)
#' library(fabletools)
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
#' future_frame

make_future <- function(fable,
                        context) {

  series_id <- context[["series_id"]]
  value_id <- context[["value_id"]]
  index_id <- context[["index_id"]]

  future_frame <- fable |>
    group_by_key() |>
    mutate(horizon = seq_len(n())) |>
    ungroup() |>
    as_tibble() |>
    select(-all_of(value_id)) |>
    rename(
      point = .mean,
      model = .model
    ) |>
    select(
      all_of(index_id),
      all_of(series_id),
      model,
      split,
      horizon,
      everything()
    )

  return(future_frame)
}
