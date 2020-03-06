
#' @title Create forecast accuracy measures.
#'
#' @description This function creates several forecast accuracy measures to evaluate point forecast. By default,
#'    the following accuracy measures are provided:
#'    \itemize{
#'       \item{ME: Mean Error}
#'       \item{MAE: Mean Absolute Error}
#'       \item{MSE: Mean Squared Error}
#'       \item{RMSE: Root Mean Squared Error}
#'       \item{MPE: Mean Percentage Error}
#'       \item{MAPE: Mean Absolute Percentage Error}
#'       }
#'
#' @param errors A tsibble in long format with standard columns (.date, .variable, .value, etc.) provided by the function \code{create_errors(...)}.
#' @param dim Character value defining the dimension for the evaluation. Possible values are "slice" or "horizon" or both.
#'
#' @return metrics A tibble with the following columns:
#'    \itemize{
#'       \item{variable: Target variable}
#'       \item{model: Forecasting model}
#'       \item{dim: Forecast accuracy by slice or by horizon}
#'       \item{num: The number of the slice or the nth-step forecast}
#'       \item{type: Training or testing}
#'       \item{metric: The accuracy metric}
#'       \item{value: Measurement variable}
#'       }
#' @export

error_metrics <- function(errors,
                          dim = c("slice", "horizon")) {

  # Exclude the last slice in errors ..........................................
  # (no test data are available resulting in NaNs)

  errors <- errors %>%
    as_tibble() %>%
    filter(!(slice) == max(errors$slice))

  if (any(dim == "horizon")) {
    # Calculate error metrics
    error_metrics <- errors %>%
      filter(type == "error") %>%
      group_by(variable, model, horizon) %>%
      summarise(
        ME = mean(value, na.rm = TRUE),
        MAE = mean(abs(value), na.rm = TRUE),
        MSE = mean(value^2, na.rm = TRUE),
        RMSE = sqrt(MSE)) %>%
      gather(key = "metric",
             value = "value",
             -c(variable, model, horizon)) %>%
      rename(num = horizon) %>%
      add_column(dim = "horizon", .before = "num") %>%
      add_column(type = "test", .after = "num") %>%
      ungroup()

    # Calculate percentage accuracy metrics
    pct_error_metrics <- errors %>%
      filter(type == "pct_error") %>%
      group_by(variable, model, horizon) %>%
      summarise(
        MPE = mean(value, na.rm = TRUE),
        MAPE = mean(abs(value), na.rm = TRUE)) %>%
      gather(key = "metric",
             value = "value",
             -c(variable, model, horizon)) %>%
      rename(num = horizon) %>%
      add_column(dim = "horizon", .before = "num") %>%
      add_column(type = "test", .after = "num") %>%
      ungroup()

    metrics_horizon <- bind_rows(
      error_metrics,
      pct_error_metrics)

  } else {
    metrics_horizon <- NULL
  }

  if (any(dim == "slice")) {
    # Calculate error metrics
    error_metrics <- errors %>%
      filter(type == "error") %>%
      group_by(variable, model, slice) %>%
      summarise(
        ME = mean(value, na.rm = TRUE),
        MAE = mean(abs(value), na.rm = TRUE),
        MSE = mean(value^2, na.rm = TRUE),
        RMSE = sqrt(MSE)) %>%
      gather(key = "metric",
             value = "value",
             -c(variable, model, slice)) %>%
      rename(num = slice) %>%
      add_column(dim = "slice", .before = "num") %>%
      add_column(type = "test", .after = "num") %>%
      ungroup()

    # Calculate percentage accuracy metrics
    pct_error_metrics <- errors %>%
      filter(type == "pct_error") %>%
      group_by(variable, model, slice) %>%
      summarise(
        MPE = mean(value, na.rm = TRUE),
        MAPE = mean(abs(value), na.rm = TRUE)) %>%
      gather(key = "metric",
             value = "value",
             -c(variable, model, slice)) %>%
      rename(num = slice) %>%
      add_column(dim = "slice", .before = "num") %>%
      add_column(type = "test", .after = "num") %>%
      ungroup()

    metrics_slice <- bind_rows(
      error_metrics,
      pct_error_metrics)

  } else {
    metrics_slice <- NULL
  }

  # Concatenate objects
  metrics <- bind_rows(
    metrics_slice,
    metrics_horizon)

  return(metrics)
}
