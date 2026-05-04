# Calculate forecast errors and percentage errors

Calculate forecast errors and percentage forecast errors for point
forecasts.

## Usage

``` r
make_errors(future_frame, main_frame, context)
```

## Arguments

- future_frame:

  A `tibble` containing the forecasts. It must contain the columns
  specified by `context`, as well as `model`, `split`, `horizon`, and
  `point`.

- main_frame:

  A `tibble` containing the observed values. It must contain the series
  identifier, time index, and value column specified by `context`.

- context:

  A named `list` with the identifiers for `series_id`, `value_id`, and
  `index_id`.

## Value

A `tibble` containing forecast errors and percentage forecast errors.

## Details

`make_errors()` compares point forecasts in `future_frame` with the
observed values in `main_frame`. The two data sets are joined by the
series identifier and time index specified in `context`.

The forecast error is calculated as `error = actual - point`. The
percentage forecast error is calculated as
`pct_error = (actual - point / point) * 100`.

Positive errors indicate that the forecast is below the observed value.
Negative errors indicate that the forecast is above the observed value.

The returned data contains:

- `series_id`: Unique identifier for the time series as specified in
  `context`.

- `model`: Forecasting model name.

- `split`: Train-test split identifier.

- `horizon`: Forecast horizon.

- `error`: Forecast error.

- `pct_error`: Percentage forecast error.

## See also

Other accuracy functions:
[`mae_vec()`](https://ahaeusser.github.io/tscv/reference/mae_vec.md),
[`make_accuracy()`](https://ahaeusser.github.io/tscv/reference/make_accuracy.md),
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

error_frame <- make_errors(
  future_frame = future_frame,
  main_frame = main_frame,
  context = context
)

error_frame
#> # A tibble: 90 × 6
#>    series model  split horizon    error pct_error
#>    <chr>  <chr>  <int>   <int>    <dbl>     <dbl>
#>  1 M14395 SNAIVE     1       1  -279.    -15.0   
#>  2 M14395 SNAIVE     1       2  -431.    -23.5   
#>  3 M14395 SNAIVE     1       3 -1624.    -99.6   
#>  4 M14395 SNAIVE     1       4  -573.    -34.5   
#>  5 M14395 SNAIVE     1       5    -1.10   -0.0708
#>  6 M14395 SNAIVE     1       6   103.     10.1   
#>  7 M14395 SNAIVE     1       7   -15.6    -2.18  
#>  8 M14395 SNAIVE     1       8  -409.    -42.8   
#>  9 M14395 SNAIVE     1       9  -257     -21.0   
#> 10 M14395 SNAIVE     1      10  -291.    -34.9   
#> # ℹ 80 more rows
```
