# Convert the forecasts from a `fable` to a `future_frame`

`make_future` converts the forecasts from a `fable` to a `future_frame`.
A `future_frame` is a tibble with a standardized format and contains the
columns:

- `index_id`: Date-time index as specified in `context`.

- `series_id`: Unique identifier for the time series as specified in
  `context`.

- `model`: Character value. The forecasting model.

- `split`: Integer value. The number of the train data split.

- `horizon`: The forecast horizon as integer.

- `point`: The point forecast as numeric value.

## Usage

``` r
make_future(fable, context)
```

## Arguments

- fable:

  A `fable` created via
  [`fabletools::forecast()`](https://generics.r-lib.org/reference/forecast.html).

- context:

  A named `list` with the identifiers for `seried_id`, `value_id` and
  `index_id`.

## Value

future_frame is a `tibble` with the forecasts.
