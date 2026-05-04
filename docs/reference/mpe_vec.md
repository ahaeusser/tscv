# Calculate the mean percentage error

Calculate the mean percentage error of a numeric vector.

## Usage

``` r
mpe_vec(truth, estimate, na_rm = TRUE)
```

## Arguments

- truth:

  Numeric vector containing the actual values.

- estimate:

  Numeric vector containing the forecasts.

- na_rm:

  Logical value. If `TRUE`, missing values are removed.

## Value

A numeric value.

## Details

`mpe_vec()` computes the average signed percentage forecast error:
`((truth - estimate) / truth) * 100`. Positive values indicate that
forecasts are, on average, below the observed values in percentage
terms. Negative values indicate that forecasts are, on average, above
the observed values.

This metric is undefined when `truth` is zero and may return `Inf`,
`-Inf`, or `NaN` in such cases.

## See also

Other accuracy functions:
[`mae_vec()`](https://ahaeusser.github.io/tscv/reference/mae_vec.md),
[`make_accuracy()`](https://ahaeusser.github.io/tscv/reference/make_accuracy.md),
[`make_errors()`](https://ahaeusser.github.io/tscv/reference/make_errors.md),
[`mape_vec()`](https://ahaeusser.github.io/tscv/reference/mape_vec.md),
[`me_vec()`](https://ahaeusser.github.io/tscv/reference/me_vec.md),
[`mse_vec()`](https://ahaeusser.github.io/tscv/reference/mse_vec.md),
[`rmse_vec()`](https://ahaeusser.github.io/tscv/reference/rmse_vec.md),
[`smape_vec()`](https://ahaeusser.github.io/tscv/reference/smape_vec.md)

## Examples

``` r
truth <- c(10, 20, 40)
estimate <- c(8, 22, 30)

mpe_vec(truth, estimate)
#> [1] 11.66667

truth_na <- c(10, 20, NA)
estimate_na <- c(8, 22, 25)
mpe_vec(truth_na, estimate_na)
#> [1] 5
```
