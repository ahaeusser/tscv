# Calculate forecast errors and percentage errors

`make_errors` calculates the forecast errors (error) and percentage
forecast errors (pct_error).

- `series_id`: Unique identifier for the time series as specified in
  `context`.

- `model`: Character value. The forecasting model.

- `split`: Integer value. The number of the train data split.

- `horizon`: The forecast horizon as integer.

- `error`: The forecast errors as numeric value.

- `pct_error`: The percentage forecast errors as numeric value.

## Usage

``` r
make_errors(future_frame, main_frame, context)
```

## Arguments

- future_frame:

  A `tibble` containing the forecasts for the models, splits, etc.

- main_frame:

  A `tibble` containing the actual values.

- context:

  A named `list` with the identifiers for `seried_id`, `value_id` and
  `index_id`.

## Value

error_frame is a `tibble` with forecast errors and percentage forecast
errors.
