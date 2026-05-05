# Estimate skewness

Estimate the skewness of a numeric distribution.

## Usage

``` r
estimate_skewness(x, na_rm = TRUE)
```

## Arguments

- x:

  Numeric vector.

- na_rm:

  Logical value. If `TRUE`, missing values are removed before
  estimation.

## Value

A numeric value giving the estimated skewness.

## Details

The function computes the moment-based skewness

\$\$ \frac{\frac{1}{n}\sum_i (x_i - \bar{x})^3} {\left(\frac{1}{n}\sum_i
(x_i - \bar{x})^2\right)^{3/2}} \$\$

Missing values are removed by default. Positive values indicate a
distribution with a longer or heavier right tail; negative values
indicate a distribution with a longer or heavier left tail.

## See also

Other data analysis:
[`acf_vec()`](https://ahaeusser.github.io/tscv/reference/acf_vec.md),
[`estimate_acf()`](https://ahaeusser.github.io/tscv/reference/estimate_acf.md),
[`estimate_kurtosis()`](https://ahaeusser.github.io/tscv/reference/estimate_kurtosis.md),
[`estimate_mode()`](https://ahaeusser.github.io/tscv/reference/estimate_mode.md),
[`estimate_pacf()`](https://ahaeusser.github.io/tscv/reference/estimate_pacf.md),
[`pacf_vec()`](https://ahaeusser.github.io/tscv/reference/pacf_vec.md),
[`summarise_data()`](https://ahaeusser.github.io/tscv/reference/summarise_data.md),
[`summarise_split()`](https://ahaeusser.github.io/tscv/reference/summarise_split.md),
[`summarise_stats()`](https://ahaeusser.github.io/tscv/reference/summarise_stats.md)

## Examples

``` r
x <- c(1, 2, 3, 4, 10, NA)

estimate_skewness(x)
#> [1] 1.13842
estimate_skewness(x, na_rm = TRUE)
#> [1] 1.13842

set.seed(123)
y <- rexp(100)
estimate_skewness(y)
#> [1] 2.790016
```
