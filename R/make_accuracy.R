
#' @title Estimate point forecast accuracy
#'
#' @description Estimate accuracy metrics for point forecasts generated from rolling-origin
#'   time series cross-validation.
#'
#' @details
#' \code{make_accuracy()} compares point forecasts in \code{future_frame} with
#' the observed values in \code{main_frame}. The two data sets are joined using
#' the series identifier and time index defined in \code{context}.
#'
#' Accuracy can be summarized along different cross-validation dimensions:
#' \itemize{
#'   \item \code{dimension = "split"} summarizes accuracy separately for each
#'   test split.
#'   \item \code{dimension = "horizon"} summarizes accuracy separately for each
#'   forecast horizon.
#' }
#'
#' The following point forecast accuracy metrics are returned:
#' \itemize{
#'   \item \code{ME}: mean error.
#'   \item \code{MAE}: mean absolute error.
#'   \item \code{MSE}: mean squared error.
#'   \item \code{RMSE}: root mean squared error.
#'   \item \code{MAPE}: mean absolute percentage error.
#'   \item \code{sMAPE}: symmetric mean absolute percentage error.
#'   \item \code{MPE}: mean percentage error.
#' }
#'
#' If \code{benchmark} is supplied, the function also computes the relative mean
#' absolute error \code{rMAE}. The \code{rMAE} is calculated as the model's
#' \code{MAE} divided by the \code{MAE} of the benchmark model for the same
#' series and selected dimension.
#'
#' @param future_frame A \code{tibble} containing point forecasts. It must
#'   contain the columns specified by \code{context}, as well as \code{model},
#'   \code{split}, \code{horizon}, and \code{point}.
#' @param main_frame A \code{tibble} containing the observed values. It must
#'   contain the series identifier, time index, and value column specified by
#'   \code{context}.
#' @param context A named \code{list} with the identifiers for
#'   \code{series_id}, \code{value_id}, and \code{index_id}.
#' @param dimension Character value. Determines the dimension over which
#'   accuracy is summarized. Common choices are \code{"split"} and
#'   \code{"horizon"}.
#' @param benchmark Optional character value giving the model name used as the
#'   benchmark for the relative mean absolute error \code{rMAE}.
#'
#' @return
#' A \code{tibble} containing the forecast accuracy metrics. The output contains
#' the series identifier, model name, selected dimension, dimension value
#' \code{n}, metric name, and metric value.
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
#' accuracy_horizon <- make_accuracy(
#'   future_frame = future_frame,
#'   main_frame = main_frame,
#'   context = context,
#'   dimension = "horizon"
#' )
#'
#' accuracy_horizon
#'
#' accuracy_split <- make_accuracy(
#'   future_frame = future_frame,
#'   main_frame = main_frame,
#'   context = context,
#'   dimension = "split"
#' )
#'
#' accuracy_split

make_accuracy <- function(future_frame,
                          main_frame,
                          context,
                          dimension = "split",
                          benchmark = NULL) {

  series_id <- context[["series_id"]]
  value_id <- context[["value_id"]]
  index_id <- context[["index_id"]]

  # Prepare test data
  main_frame <- main_frame |>
    rename(actual = all_of(value_id))

  # Join main_frame (test data) and future_frame (forecasts)
  data <- left_join(
    x = future_frame,
    y = main_frame,
    by = c(series_id, index_id)
  ) |>
    select(
      all_of(c(series_id, "model", "split", "horizon", "point", "actual"))
    )

  # Estimate common accuracy metrics
  accuracy_frame <- data |>
    group_by(
      !!sym(series_id),
      .data$model,
      .data[[dimension]]
    ) |>
    summarise(
      ME = me_vec(truth = .data$actual, estimate = .data$point),
      MAE = mae_vec(truth = .data$actual, estimate = .data$point),
      MSE = mse_vec(truth = .data$actual, estimate = .data$point),
      RMSE = rmse_vec(truth = .data$actual, estimate = .data$point),
      MAPE = mape_vec(truth = .data$actual, estimate = .data$point),
      sMAPE = smape_vec(truth = .data$actual, estimate = .data$point),
      MPE = mpe_vec(truth = .data$actual, estimate = .data$point),
      .groups = "drop"
    ) |>
    arrange(
      !!sym(series_id),
      .data$model,
      .data[[dimension]]
    )

  column_all <- names(accuracy_frame)
  column_drop <- c(series_id, "model", dimension)
  set_metrics <- column_all[!column_all %in% column_drop]

  accuracy_frame <- accuracy_frame |>
    pivot_longer(
      cols = all_of(set_metrics),
      names_to = "metric",
      values_to = "value"
    ) |>
    arrange(
      !!sym(series_id),
      .data$model,
      .data$metric
    )

  if (!is.null(benchmark)) {

    set_models <- unique(accuracy_frame$model)

    mae_benchmark <- accuracy_frame |>
      filter(.data$metric == "MAE") |>
      filter(.data$model == benchmark) |>
      pivot_wider(
        names_from = all_of("model"),
        values_from = all_of("value")
      )

    mae_benchmark <- map_dfr(
      .x = seq_along(set_models),
      .f = ~{
        mae_benchmark |>
          mutate(model = set_models[.x])
      }
    )

    metrics_rmae <- left_join(
      x = filter(accuracy_frame, .data$metric == "MAE"),
      y = mae_benchmark,
      by = c(series_id, dimension, "metric", "model")
    ) |>
      mutate(value = .data$value / .data[[benchmark]]) |>
      mutate(metric = "rMAE") |>
      select(-all_of(benchmark))

    accuracy_frame <- bind_rows(
      accuracy_frame,
      metrics_rmae
    ) |>
      arrange(
        !!sym(series_id),
        .data$model,
        .data$metric
      )
  }

  accuracy_frame <- accuracy_frame |>
    mutate(dimension = dimension, .after = "model") |>
    rename(n = all_of(dimension))

  return(accuracy_frame)
}

