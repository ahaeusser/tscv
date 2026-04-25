# Summary statistics for time series data

This function calculates several summary statistics for time series
data. The function calculates the following metrics for all key
combinations:

- `start`: Start date (or date-time)

- `end`: End date (or date-time)

- `n_obs`: Number of observations per time series

- `n_missing`: Number of missing values (NAs)

- `pct_missing`: Percentage rate of missing values

- `n_zeros`: Number of zero values

- `pct_zeros`: Percentage rate of zero values

## Usage

``` r
summarise_data(.data, context)
```

## Arguments

- .data:

  A `tibble` in long format containing time series data.

- context:

  A named `list` with the identifiers for `seried_id`, `value_id` and
  `index_id`.

## Value

data A tibble containing the summary statistics.
