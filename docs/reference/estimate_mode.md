# Estimate the mode of a distribution

Estimate the mode of a numeric distribution using kernel density
estimation.

## Usage

``` r
estimate_mode(x, na_rm = TRUE, ...)
```

## Arguments

- x:

  Numeric vector.

- na_rm:

  Logical value. If `TRUE`, missing values are removed before
  estimation.

- ...:

  Further arguments passed to
  [`stats::density()`](https://rdrr.io/r/stats/density.html).

## Value

A numeric value giving the estimated mode of the distribution.

## Details

The function computes a kernel density estimate with
[`stats::density()`](https://rdrr.io/r/stats/density.html) and returns
the value of `x` at which the estimated density is largest.

Missing values are removed by default. Additional arguments are passed
to [`stats::density()`](https://rdrr.io/r/stats/density.html), for
example `bw`, `kernel`, or `n`.

## See also

Other data analysis:
[`acf_vec()`](https://ahaeusser.github.io/tscv/reference/acf_vec.md),
[`estimate_acf()`](https://ahaeusser.github.io/tscv/reference/estimate_acf.md),
[`estimate_kurtosis()`](https://ahaeusser.github.io/tscv/reference/estimate_kurtosis.md),
[`estimate_pacf()`](https://ahaeusser.github.io/tscv/reference/estimate_pacf.md),
[`estimate_skewness()`](https://ahaeusser.github.io/tscv/reference/estimate_skewness.md),
[`pacf_vec()`](https://ahaeusser.github.io/tscv/reference/pacf_vec.md),
[`summarise_data()`](https://ahaeusser.github.io/tscv/reference/summarise_data.md),
[`summarise_split()`](https://ahaeusser.github.io/tscv/reference/summarise_split.md),
[`summarise_stats()`](https://ahaeusser.github.io/tscv/reference/summarise_stats.md)

## Examples

``` r
x <- c(1, 1, 2, 2, 2, 3, 4, NA)

estimate_mode(x)
#> [1] 1.956091
estimate_mode(x, na_rm = TRUE)
#> [1] 1.956091
estimate_mode(x, bw = "nrd0")
#> [1] 1.956091

set.seed(123)
y <- rnorm(100, mean = 5)
estimate_mode(y)
#> [1] 4.807602
```
