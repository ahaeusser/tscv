
#' @title Estimate accuracy metrics to evaluate point forecast
#'
#' @description This function estimates several accuracy metrics to evaluate
#'   the accuracy of point forecasts. Either along the forecast horizon or
#'   along the train-test-splits. By default, the following accuracy metrics
#'   are provided:
#'
#'    \itemize{
#'       \item{\code{ME}: mean error}
#'       \item{\code{MAE}: mean absolute error}
#'       \item{\code{MSE}: mean squared error}
#'       \item{\code{RMSE}: root mean squared error}
#'       \item{\code{MAPE}: mean absolute percentage error}
#'       \item{\code{sMAPE}: scaled mean absolute percentage error}
#'       \item{\code{MPE}: mean percentage error}
#'       \item{\code{rMAE}: relative mean absolute error}
#'       }
#'
#' @param future A \code{tibble} containing the forecasts for the models, splits, etc.
#' @param main A \code{tibble} containing the actual values.
#' @param dimension Character value. The forecast accuracy is estimated by \code{split} or \code{horizon}.
#' @param benchmark Character value. The forecast model used as benchmark for the relative mean absolute error (rMAE).
#'
#' @return A \code{tibble} containing the accuracy metrics for all series
#'    models etc.
#' @export

make_accuracy <- function(future,
                          main,
                          dimension = "split",
                          benchmark = NULL) {

  series_id <- context[["series_id"]]
  value_id <- context[["value_id"]]
  index_id <- context[["index_id"]]

  # Prepare test data
  main <- rename(
    .data = main,
    actual = !!sym(value_id)
  )

  # Join main_frame (test data) and future_frame (forecasts)
  data <- left_join(
    x = future,
    y = main,
    by = c(series_id, index_id)) %>%
    select(c(!!sym(series_id), "model", "split", "horizon", "point", "actual"))

  # Estimate common accuracy metrics
  metrics <- data %>%
    group_by(!!sym(series_id), .data$model, !!sym(dimension)) %>%
    summarise(
      ME = me_vec(truth = .data$actual, estimate = .data$point),
      MAE = mae_vec(truth = .data$actual, estimate = .data$point),
      MSE = mse_vec(truth = .data$actual, estimate = .data$point),
      RMSE = rmse_vec(truth = .data$actual, estimate = .data$point),
      MAPE = mape_vec(truth = .data$actual, estimate = .data$point),
      sMAPE = smape_vec(truth = .data$actual, estimate = .data$point),
      MPE = mpe_vec(truth = .data$actual, estimate = .data$point),
      .groups = "drop") %>%
    arrange(!!sym(series_id), .data$model, !!sym(dimension))

  column_all <- names(metrics)
  column_drop <- c(series_id, "model", dimension)
  set_metrics <- column_all[!column_all %in% column_drop]

  metrics <- metrics %>%
    pivot_longer(
      cols = all_of(set_metrics),
      names_to = "metric",
      values_to = "value") %>%
    arrange(!!sym(series_id), .data$model, .data$metric)


  if (!is.null(benchmark)) {

    set_models <- unique(metrics$model)

    mae_benchmark <- metrics %>%
      filter(metric == "MAE") %>%
      filter(model == benchmark) %>%
      pivot_wider(
        names_from = model,
        values_from = value
      )

    mae_benchmark <- map_dfr(
      .x = 1:length(set_models),
      .f = ~{
        mae_benchmark %>%
          mutate(model = set_models[.x])
      }
    )

    metrics_rmae <- left_join(
      x = filter(metrics, metric == "MAE"),
      y = mae_benchmark,
      by = c(series_id, dimension, "metric", "model")) %>%
      mutate(value = value / !!sym(benchmark)) %>%
      mutate(metric = "rMAE") %>%
      select(-!!sym(benchmark))

    metrics <- bind_rows(
      metrics,
      metrics_rmae) %>%
      arrange(!!sym(series_id), .data$model, .data$metric)

  }

  metrics <- metrics %>%
    mutate(dimension = dimension, .after = "model") %>%
    rename(n = !!sym(dimension))

  return(metrics)
}
