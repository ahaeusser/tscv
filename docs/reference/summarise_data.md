# Summarise time series data

Calculate basic data-quality summary statistics for one or more time
series.

## Usage

``` r
summarise_data(.data, context)
```

## Arguments

- .data:

  A `tibble` in long format containing time series data.

- context:

  A named `list` with the identifiers for `series_id`, `value_id`, and
  `index_id`.

## Value

A `tibble` containing one row per time series and the calculated summary
statistics.

## Details

`summarise_data()` groups the input data by the series identifier
supplied in `context` and returns one row per time series.

The function reports:

- `start`: first time index;

- `end`: last time index;

- `n_obs`: number of observations;

- `n_missing`: number of missing values;

- `pct_missing`: percentage of missing values;

- `n_zeros`: number of zero values;

- `pct_zeros`: percentage of zero values.

## See also

Other data analysis:
[`acf_vec()`](https://ahaeusser.github.io/tscv/reference/acf_vec.md),
[`estimate_acf()`](https://ahaeusser.github.io/tscv/reference/estimate_acf.md),
[`estimate_kurtosis()`](https://ahaeusser.github.io/tscv/reference/estimate_kurtosis.md),
[`estimate_mode()`](https://ahaeusser.github.io/tscv/reference/estimate_mode.md),
[`estimate_pacf()`](https://ahaeusser.github.io/tscv/reference/estimate_pacf.md),
[`estimate_skewness()`](https://ahaeusser.github.io/tscv/reference/estimate_skewness.md),
[`pacf_vec()`](https://ahaeusser.github.io/tscv/reference/pacf_vec.md),
[`summarise_split()`](https://ahaeusser.github.io/tscv/reference/summarise_split.md),
[`summarise_stats()`](https://ahaeusser.github.io/tscv/reference/summarise_stats.md)

## Examples

``` r
library(dplyr)

context <- list(
  series_id = "series",
  value_id = "value",
  index_id = "index"
)

data <- M4_monthly_data |>
  filter(series %in% c("M23100", "M14395"))

summarise_data(
  .data = data,
  context = context
)
#> # A tibble: 2 × 8
#>   series    start      end n_obs n_missing pct_missing n_zeros pct_zeros
#>   <chr>     <mth>    <mth> <int>     <int>       <dbl>   <int>     <dbl>
#> 1 M14395 2001 Jul 2016 Dez   186         0           0       0         0
#> 2 M23100 2003 Jan 2016 Dez   168         0           0       0         0
```
