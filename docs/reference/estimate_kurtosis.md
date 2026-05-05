# Estimate kurtosis

Estimate the kurtosis of a numeric distribution.

## Usage

``` r
estimate_kurtosis(x, na_rm = TRUE)
```

## Arguments

- x:

  Numeric vector.

- na_rm:

  Logical value. If `TRUE`, missing values are removed before
  estimation.

## Value

A numeric value giving the estimated kurtosis.

## Details

The function computes the moment-based kurtosis

\$\$ \frac{n \sum_i (x_i - \bar{x})^4} {\left(\sum_i (x_i -
\bar{x})^2\right)^2} \$\$

Missing values are removed by default.

This returns the usual kurtosis, not excess kurtosis. A normal
distribution has kurtosis close to `3`.

## See also

Other data analysis:
[`acf_vec()`](https://ahaeusser.github.io/tscv/reference/acf_vec.md),
[`estimate_acf()`](https://ahaeusser.github.io/tscv/reference/estimate_acf.md),
[`estimate_mode()`](https://ahaeusser.github.io/tscv/reference/estimate_mode.md),
[`estimate_pacf()`](https://ahaeusser.github.io/tscv/reference/estimate_pacf.md),
[`estimate_skewness()`](https://ahaeusser.github.io/tscv/reference/estimate_skewness.md),
[`pacf_vec()`](https://ahaeusser.github.io/tscv/reference/pacf_vec.md),
[`summarise_data()`](https://ahaeusser.github.io/tscv/reference/summarise_data.md),
[`summarise_split()`](https://ahaeusser.github.io/tscv/reference/summarise_split.md),
[`summarise_stats()`](https://ahaeusser.github.io/tscv/reference/summarise_stats.md)

## Examples

``` r
x <- c(1, 2, 3, 4, 5, NA)

estimate_kurtosis(x)
#> [1] 1.7
estimate_kurtosis(x, na_rm = TRUE)
#> [1] 1.7

set.seed(123)
y <- rnorm(100)
estimate_kurtosis(y)
#> [1] 2.838947
```
