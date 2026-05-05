# Estimate autocorrelations by time series

Estimate the sample autocorrelation function for one or more time series
in a `tibble`.

## Usage

``` r
estimate_acf(.data, context, lag_max = 24, level = 0.9, ...)
```

## Arguments

- .data:

  A `tibble` containing the time series data.

- context:

  A named `list` with the identifiers for `series_id`, `value_id`, and
  `index_id`.

- lag_max:

  Integer. Maximum lag for which the autocorrelation is estimated.

- level:

  Numeric value. Confidence level used to calculate the approximate
  significance bound.

- ...:

  Further arguments passed to
  [`stats::acf()`](https://rdrr.io/r/stats/acf.html).

## Value

A `tibble` with the series identifier and the columns `type`, `lag`,
`value`, `bound`, and `sign`.

## Details

`estimate_acf()` groups the input data by the series identifier supplied
in `context` and estimates the sample autocorrelation function for each
time series separately.

The output contains one row per series and lag. The column `bound`
contains an approximate significance threshold based on the selected
confidence level. The logical column `sign` indicates whether the
absolute autocorrelation is larger than this threshold.

## See also

Other data analysis:
[`acf_vec()`](https://ahaeusser.github.io/tscv/reference/acf_vec.md),
[`estimate_kurtosis()`](https://ahaeusser.github.io/tscv/reference/estimate_kurtosis.md),
[`estimate_mode()`](https://ahaeusser.github.io/tscv/reference/estimate_mode.md),
[`estimate_pacf()`](https://ahaeusser.github.io/tscv/reference/estimate_pacf.md),
[`estimate_skewness()`](https://ahaeusser.github.io/tscv/reference/estimate_skewness.md),
[`pacf_vec()`](https://ahaeusser.github.io/tscv/reference/pacf_vec.md),
[`summarise_data()`](https://ahaeusser.github.io/tscv/reference/summarise_data.md),
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

estimate_acf(
  .data = data,
  context = context,
  lag_max = 12
)
#> # A tibble: 24 × 6
#>    series type    lag  value bound sign 
#>    <chr>  <chr> <int>  <dbl> <dbl> <lgl>
#>  1 M14395 ACF       1 0.675  0.121 TRUE 
#>  2 M14395 ACF       2 0.410  0.121 TRUE 
#>  3 M14395 ACF       3 0.253  0.121 TRUE 
#>  4 M14395 ACF       4 0.129  0.121 TRUE 
#>  5 M14395 ACF       5 0.0847 0.121 FALSE
#>  6 M14395 ACF       6 0.0448 0.121 FALSE
#>  7 M14395 ACF       7 0.0246 0.121 FALSE
#>  8 M14395 ACF       8 0.0732 0.121 FALSE
#>  9 M14395 ACF       9 0.149  0.121 TRUE 
#> 10 M14395 ACF      10 0.282  0.121 TRUE 
#> # ℹ 14 more rows
```
