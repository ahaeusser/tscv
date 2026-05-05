# Estimate autocorrelations of a numeric vector

Estimate the sample autocorrelation function of a numeric vector.

## Usage

``` r
acf_vec(x, lag_max = 24, ...)
```

## Arguments

- x:

  Numeric vector.

- lag_max:

  Integer. Maximum lag for which the autocorrelation is estimated.

- ...:

  Further arguments passed to
  [`stats::acf()`](https://rdrr.io/r/stats/acf.html).

## Value

A numeric vector containing the sample autocorrelations for lags `1` to
`lag_max`.

## Details

`acf_vec()` is a small wrapper around
[`stats::acf()`](https://rdrr.io/r/stats/acf.html). It returns the
sample autocorrelations as a numeric vector and removes lag 0 from the
output, because lag 0 is always equal to 1 and is usually not needed for
diagnostics.

## See also

Other data analysis:
[`estimate_acf()`](https://ahaeusser.github.io/tscv/reference/estimate_acf.md),
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
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

x <- M4_monthly_data |>
  filter(series == first(series)) |>
  pull(value)

acf_vec(
  x = x,
  lag_max = 12
)
#>  [1] 0.9929581 0.9868445 0.9807885 0.9746906 0.9692508 0.9631796 0.9573904
#>  [8] 0.9512882 0.9460969 0.9404290 0.9346511 0.9292045
```
