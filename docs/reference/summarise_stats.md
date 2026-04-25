# Summary statistics for time series data

This function calculates several summary statistics for time series
data. The function calculates the following metrics for all key
combinations:

- `mean`: Arithmetic mean

- `median`: Median (p50)

- `mode`: Mode

- `sd`: Standard deviation

- `p0`: Minimum

- `p25`: 25%-Quantile

- `p75`: 75%-Quantile

- `p100`: Maximum

- `skewness`: Skewness

- `kurtosis`: Kurtosis

## Usage

``` r
summarise_stats(.data, context)
```

## Arguments

- .data:

  A `tibble` in long format containing time series data.

- context:

  A named `list` with the identifiers for `seried_id`, `value_id` and
  `index_id`.

## Value

data A tibble containing the summary statistics.
