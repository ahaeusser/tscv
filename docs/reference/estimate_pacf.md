# Estimate the sample partial autocorrelation

The function estimates the sample partial autocorrelation function of a
`tibble` containing several time series.

## Usage

``` r
estimate_pacf(.data, context, lag_max = 24, level = 0.9, ...)
```

## Arguments

- .data:

  A `tibble` containing the time series data.

- context:

  A named `list` with the identifiers for `seried_id`, `value_id` and
  `index_id`.

- lag_max:

  Maximum lag as integer.

- level:

  Numeric value. The confidence level to check significance.

- ...:

  Further arguments passed to
  [`stats::pacf()`](https://rdrr.io/r/stats/acf.html).

## Value

data A `tibble` with a column for the unique identifier of the time
series and the columns type, lag and value.
