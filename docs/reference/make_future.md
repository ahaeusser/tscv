# Convert forecasts to a future frame

Convert forecasts from a `fable` object to a standardized forecast
table.

## Usage

``` r
make_future(fable, context)
```

## Arguments

- fable:

  A `fable` created with
  [`forecast()`](https://generics.r-lib.org/reference/forecast.html).

- context:

  A named `list` with the identifiers for `series_id`, `value_id`, and
  `index_id`.

## Value

A `tibble` containing forecasts in standardized `future_frame` format.

## Details

`make_future()` converts the output of
[`forecast()`](https://generics.r-lib.org/reference/forecast.html) into
a `tibble` with a consistent structure for downstream evaluation,
plotting, and accuracy calculation.

The returned `future_frame` contains one row per forecasted observation,
time series, split, and model. It includes the following columns:

- the time index column specified by `context$index_id`;

- the series identifier column specified by `context$series_id`;

- `model`: the forecasting model name;

- `split`: the train-test split identifier;

- `horizon`: the forecast horizon within each series, split, and model;

- `point`: the point forecast, taken from the `.mean` column of the
  `fable`.

This format is used by functions such as
[`make_accuracy()`](https://ahaeusser.github.io/tscv/reference/make_accuracy.md)
and
[`make_errors()`](https://ahaeusser.github.io/tscv/reference/make_errors.md).

## See also

Other time series cross-validation:
[`make_split()`](https://ahaeusser.github.io/tscv/reference/make_split.md),
[`make_tsibble()`](https://ahaeusser.github.io/tscv/reference/make_tsibble.md),
[`slice_test()`](https://ahaeusser.github.io/tscv/reference/slice_test.md),
[`slice_train()`](https://ahaeusser.github.io/tscv/reference/slice_train.md),
[`split_index()`](https://ahaeusser.github.io/tscv/reference/split_index.md)

## Examples

``` r
library(dplyr)
library(tsibble)
library(fable)
library(fabletools)

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

future_frame
#> # A tibble: 90 × 6
#>       index series model  split horizon point
#>       <mth> <chr>  <chr>  <int>   <int> <dbl>
#>  1 2011 Jul M14395 SNAIVE     1       1 2145.
#>  2 2011 Aug M14395 SNAIVE     1       2 2264.
#>  3 2011 Sep M14395 SNAIVE     1       3 3253 
#>  4 2011 Okt M14395 SNAIVE     1       4 2232.
#>  5 2011 Nov M14395 SNAIVE     1       5 1556.
#>  6 2011 Dez M14395 SNAIVE     1       6  915.
#>  7 2012 Jan M14395 SNAIVE     1       7  732.
#>  8 2012 Feb M14395 SNAIVE     1       8 1367 
#>  9 2012 Mrz M14395 SNAIVE     1       9 1478.
#> 10 2012 Apr M14395 SNAIVE     1      10 1122.
#> # ℹ 80 more rows
```
