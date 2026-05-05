# Estimate partial autocorrelations by time series

Estimate the sample partial autocorrelation function for one or more
time series in a `tibble`.

## Usage

``` r
estimate_pacf(.data, context, lag_max = 24, level = 0.9, ...)
```

## Arguments

- .data:

  A `tibble` containing the time series data.

- context:

  A named `list` with the identifiers for `series_id`, `value_id`, and
  `index_id`.

- lag_max:

  Integer. Maximum lag for which the partial autocorrelation is
  estimated.

- level:

  Numeric value. Confidence level used to calculate the approximate
  significance bound.

- ...:

  Further arguments passed to
  [`stats::pacf()`](https://rdrr.io/r/stats/acf.html).

## Value

A `tibble` with the series identifier and the columns `type`, `lag`,
`value`, `bound`, and `sign`.

## Details

`estimate_pacf()` groups the input data by the series identifier
supplied in `context` and estimates the sample partial autocorrelation
function for each time series separately.

The output contains one row per series and lag. The column `bound`
contains an approximate significance threshold based on the selected
confidence level. The logical column `sign` indicates whether the
absolute partial autocorrelation is larger than this threshold.

## See also

Other data analysis:
[`acf_vec()`](https://ahaeusser.github.io/tscv/reference/acf_vec.md),
[`estimate_acf()`](https://ahaeusser.github.io/tscv/reference/estimate_acf.md),
[`estimate_kurtosis()`](https://ahaeusser.github.io/tscv/reference/estimate_kurtosis.md),
[`estimate_mode()`](https://ahaeusser.github.io/tscv/reference/estimate_mode.md),
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

estimate_pacf(
  .data = data,
  context = context,
  lag_max = 12
)
#> # A tibble: 24 × 6
#>    series type    lag   value bound sign 
#>    <chr>  <chr> <int>   <dbl> <dbl> <lgl>
#>  1 M14395 PACF      1  0.675  0.121 TRUE 
#>  2 M14395 PACF      2 -0.0826 0.121 FALSE
#>  3 M14395 PACF      3  0.0163 0.121 FALSE
#>  4 M14395 PACF      4 -0.0518 0.121 FALSE
#>  5 M14395 PACF      5  0.0515 0.121 FALSE
#>  6 M14395 PACF      6 -0.0321 0.121 FALSE
#>  7 M14395 PACF      7  0.0129 0.121 FALSE
#>  8 M14395 PACF      8  0.104  0.121 FALSE
#>  9 M14395 PACF      9  0.108  0.121 FALSE
#> 10 M14395 PACF     10  0.213  0.121 TRUE 
#> # ℹ 14 more rows
```
