
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
#'       \item{\code{MASE}: mean absolute scaled error}
#'       \item{\code{sMASE}: seasonal mean absolute scaled error}
#'       }
#'
#' @param fcst A \code{fable} containing the forecasts for the models, splits, etc.
#' @param data A \code{tsibble} containing the training and testing data.
#' @param period Integer value. The period used for the estimation of the in-sample
#'    MAE of seasonal naive forecast. The in-sample MAE is required for scaling the sMASE.
#' @param by Character value. Either accuracy is estimated by \code{split} or \code{horizon}.
#'
#' @return A \code{tibble} containing the accuracy metrics for all key variables and models.
#' @export

error_metrics <- function(fcst,
                          data,
                          period = NULL,
                          by = "split") {

  dttm <- index_var(fcst)
  target <- target_vars(fcst)
  value <- value_var(fcst)

  if (is_empty(period)) {
    period <- min(common_periods(data))
  }

  # Prepare train and test data
  # In-sample MAE of seasonal naive is required for scaling of MASE
  train <- data %>%
    filter(.data$sample == "train") %>%
    rename(actual = !!sym(value))

  test <- data %>%
    filter(.data$sample == "test") %>%
    rename(actual = !!sym(value))

  # Estimate in-sample MAE for scaling of sMASE
  mae_train <- train %>%
    as_tibble() %>%
    group_by(!!!syms(target), .data$split) %>%
    mutate(lagged = dplyr::lag(.data$actual, n = period)) %>%
    summarise(
      mae_train = mae_vec(
        truth = .data$actual,
        estimate = .data$lagged)) %>%
    ungroup()

  # Join in-sample MAE to test data
  test <- left_join(
    x = test,
    y = mae_train,
    by = c(target, "split")
  )

  # Extract point forecasts
  fcst <- fcst %>%
    as_tsibble() %>%
    mutate(!!sym(value) := map_dbl(fcst[[value]], `[[`, "mu"))

  # Join test and forecast data
  data <- left_join(
    x = fcst,
    y = test,
    by = c(target, "split", dttm)
  )

  # Estimate common accuracy metrics
  metrics <- data %>%
    as_tibble() %>%
    group_by(!!!syms(target), .data$.model, !!sym(by)) %>%
    summarise(
      ME = me_vec(truth = .data$actual, estimate = !!sym(value)),
      MAE = mae_vec(truth = .data$actual, estimate = !!sym(value)),
      MSE = mse_vec(truth = .data$actual, estimate = !!sym(value)),
      RMSE = rmse_vec(truth = .data$actual, estimate = !!sym(value)),
      MAPE = mape_vec(truth = .data$actual, estimate = !!sym(value)),
      sMAPE = smape_vec(truth = .data$actual, estimate = !!sym(value)),
      MPE = mpe_vec(truth = .data$actual, estimate = !!sym(value))) %>%
    arrange(!!!syms(target), .data$.model, !!sym(by)) %>%
    ungroup()

  # Estimate seasonal MASE
  mase <- data %>%
    as_tibble() %>%
    mutate(q = (.data$actual - !!sym(value)) / mae_train) %>%
    group_by(!!!syms(target), .data$.model, !!sym(by)) %>%
    summarise(
      MASE = mean(abs(q), na.rm = TRUE)) %>%
    ungroup()

  metrics <- left_join(
    x = metrics,
    y = mase,
    by = c(target, ".model", by)
  )

  set_metrics <- c(
    "ME", "MAE", "MSE", "RMSE", "MAPE", "sMAPE", "MPE", "MASE")

  metrics <- metrics %>%
    pivot_longer(
      cols = all_of(set_metrics),
      names_to = "metric",
      values_to = "value") %>%
    arrange(!!!syms(target), .data$.model, .data$metric) %>%
    mutate(dimension = by, .after = ".model") %>%
    rename(n = !!sym(by))

  return(metrics)
}
