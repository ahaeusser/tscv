# Estimate partial autocorrelations of a numeric vector

Estimate the sample partial autocorrelation function of a numeric
vector.

## Usage

``` r
pacf_vec(x, lag_max = 24, ...)
```

## Arguments

- x:

  Numeric vector.

- lag_max:

  Integer. Maximum lag for which the partial autocorrelation is
  estimated.

- ...:

  Further arguments passed to
  [`stats::pacf()`](https://rdrr.io/r/stats/acf.html).

## Value

A numeric vector containing the sample partial autocorrelations for lags
`1` to `lag_max`.

## Details

`pacf_vec()` is a small wrapper around
[`stats::pacf()`](https://rdrr.io/r/stats/acf.html). It returns the
sample partial autocorrelations as a numeric vector for lags `1` to
`lag_max`.

## See also

Other data analysis:
[`acf_vec()`](https://ahaeusser.github.io/tscv/reference/acf_vec.md),
[`estimate_acf()`](https://ahaeusser.github.io/tscv/reference/estimate_acf.md),
[`estimate_kurtosis()`](https://ahaeusser.github.io/tscv/reference/estimate_kurtosis.md),
[`estimate_mode()`](https://ahaeusser.github.io/tscv/reference/estimate_mode.md),
[`estimate_pacf()`](https://ahaeusser.github.io/tscv/reference/estimate_pacf.md),
[`estimate_skewness()`](https://ahaeusser.github.io/tscv/reference/estimate_skewness.md),
[`summarise_data()`](https://ahaeusser.github.io/tscv/reference/summarise_data.md),
[`summarise_split()`](https://ahaeusser.github.io/tscv/reference/summarise_split.md),
[`summarise_stats()`](https://ahaeusser.github.io/tscv/reference/summarise_stats.md)

## Examples

``` r
library(dplyr)

x <- M4_monthly_data |>
  filter(series == first(series)) |>
  pull(value)

pacf_vec(
  x = x,
  lag_max = 12
)
#>  [1]  0.992958082  0.062614262  0.005388978 -0.005221888  0.043685682
#>  [6] -0.041864960  0.012261946 -0.023825645  0.062006799 -0.033049289
#> [11] -0.010337346  0.015735471
```
