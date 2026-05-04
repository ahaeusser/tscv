# Estimate point forecast accuracy

Estimate accuracy metrics for point forecasts generated from
rolling-origin time series cross-validation.

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

  A `tibble` containing point forecasts. It must contain the columns
  specified by `context`, as well as `model`, `split`, `horizon`, and
  `point`.

- main_frame:

  A `tibble` containing the observed values. It must contain the series
  identifier, time index, and value column specified by `context`.

- context:

  A named `list` with the identifiers for `series_id`, `value_id`, and
  `index_id`.

- dimension:

  Character value. Determines the dimension over which accuracy is
  summarized. Common choices are `"split"` and `"horizon"`.

- benchmark:

  Optional character value giving the model name used as the benchmark
  for the relative mean absolute error `rMAE`.

## Value

A `tibble` containing the forecast accuracy metrics. The output contains
the series identifier, model name, selected dimension, dimension value
`n`, metric name, and metric value.

## Details

`make_accuracy()` compares point forecasts in `future_frame` with the
observed values in `main_frame`. The two data sets are joined using the
series identifier and time index defined in `context`.

Accuracy can be summarized along different cross-validation dimensions:

- `dimension = "split"` summarizes accuracy separately for each test
  split.

- `dimension = "horizon"` summarizes accuracy separately for each
  forecast horizon.

The following point forecast accuracy metrics are returned:

- `ME`: mean error.

- `MAE`: mean absolute error.

- `MSE`: mean squared error.

- `RMSE`: root mean squared error.

- `MAPE`: mean absolute percentage error.

- `sMAPE`: symmetric mean absolute percentage error.

- `MPE`: mean percentage error.

If `benchmark` is supplied, the function also computes the relative mean
absolute error `rMAE`. The `rMAE` is calculated as the model's `MAE`
divided by the `MAE` of the benchmark model for the same series and
selected dimension.

## See also

Other accuracy functions:
[`mae_vec()`](https://ahaeusser.github.io/tscv/reference/mae_vec.md),
[`make_errors()`](https://ahaeusser.github.io/tscv/reference/make_errors.md),
[`mape_vec()`](https://ahaeusser.github.io/tscv/reference/mape_vec.md),
[`me_vec()`](https://ahaeusser.github.io/tscv/reference/me_vec.md),
[`mpe_vec()`](https://ahaeusser.github.io/tscv/reference/mpe_vec.md),
[`mse_vec()`](https://ahaeusser.github.io/tscv/reference/mse_vec.md),
[`rmse_vec()`](https://ahaeusser.github.io/tscv/reference/rmse_vec.md),
[`smape_vec()`](https://ahaeusser.github.io/tscv/reference/smape_vec.md)

## Examples

``` r
library(dplyr)
library(tsibble)
library(fabletools)
library(fable)

context <- list(
  series_id = "series",
  value_id = "value",
  index_id = "index"
)

main_frame <- M4_monthly_data |>
  filter(series %in% c("M23100", "M14395"))

split_frame <- make_split(
  main_frame = main_frame,
  context = context,
  type = "first",
  value = 120,
  n_ahead = 18,
  n_skip = 17,
  n_lag = 0,
  mode = "stretch",
  exceed = FALSE
)

train_frame <- slice_train(
  main_frame = main_frame,
  split_frame = split_frame,
  context = context
) |>
  as_tsibble(
    index = index,
    key = c(series, split)
  )

model_frame <- train_frame |>
  model(
    "SNAIVE" = SNAIVE(value ~ lag("year"))
  )

fable_frame <- model_frame |>
  forecast(h = 18)

future_frame <- make_future(
  fable = fable_frame,
  context = context
)

accuracy_horizon <- make_accuracy(
  future_frame = future_frame,
  main_frame = main_frame,
  context = context,
  dimension = "horizon"
)

accuracy_horizon
#> # A tibble: 252 × 6
#>    series model  dimension     n metric value
#>    <chr>  <chr>  <chr>     <int> <chr>  <dbl>
#>  1 M14395 SNAIVE horizon       1 MAE    193. 
#>  2 M14395 SNAIVE horizon       2 MAE    289. 
#>  3 M14395 SNAIVE horizon       3 MAE    811. 
#>  4 M14395 SNAIVE horizon       4 MAE    653. 
#>  5 M14395 SNAIVE horizon       5 MAE     80.9
#>  6 M14395 SNAIVE horizon       6 MAE     90.4
#>  7 M14395 SNAIVE horizon       7 MAE     18.3
#>  8 M14395 SNAIVE horizon       8 MAE    487. 
#>  9 M14395 SNAIVE horizon       9 MAE    463. 
#> 10 M14395 SNAIVE horizon      10 MAE    185. 
#> # ℹ 242 more rows

accuracy_split <- make_accuracy(
  future_frame = future_frame,
  main_frame = main_frame,
  context = context,
  dimension = "split"
)

accuracy_split
#> # A tibble: 35 × 6
#>    series model  dimension     n metric  value
#>    <chr>  <chr>  <chr>     <int> <chr>   <dbl>
#>  1 M14395 SNAIVE split         1 MAE     489. 
#>  2 M14395 SNAIVE split         2 MAE     179. 
#>  3 M14395 SNAIVE split         3 MAE     499. 
#>  4 M14395 SNAIVE split         1 MAPE     36.8
#>  5 M14395 SNAIVE split         2 MAPE     13.8
#>  6 M14395 SNAIVE split         3 MAPE     24.2
#>  7 M14395 SNAIVE split         1 ME     -453. 
#>  8 M14395 SNAIVE split         2 ME       95.0
#>  9 M14395 SNAIVE split         3 ME      499. 
#> 10 M14395 SNAIVE split         1 MPE     -34.1
#> # ℹ 25 more rows
```
