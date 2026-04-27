# Estimate accuracy metrics to evaluate point forecast

The function estimates several accuracy metrics to evaluate the accuracy
of point forecasts. Either along the forecast horizon or along the
test-splits. By default, the following accuracy metrics are provided:

- `ME`: mean error

- `MAE`: mean absolute error

- `MSE`: mean squared error

- `RMSE`: root mean squared error

- `MAPE`: mean absolute percentage error

- `sMAPE`: symmetric mean absolute percentage error

- `MPE`: mean percentage error

- `rMAE`: relative mean absolute error

## Usage

``` r
make_accuracy(
  future_frame,
  main_frame,
  context,
  dimension = "split",
  benchmark = NULL
)
```

## Arguments

- future_frame:

  A `tibble` containing the forecasts for the models, splits, etc.

- main_frame:

  A `tibble` containing the actual values.

- context:

  A named `list` with the identifiers for `seried_id`, `value_id` and
  `index_id`.

- dimension:

  Character value. The forecast accuracy is estimated by `split` or
  `horizon`.

- benchmark:

  Character value. The forecast model used as benchmark for the relative
  mean absolute error (rMAE).

## Value

accuracy_frame is `tibble` containing the accuracy metrics.
