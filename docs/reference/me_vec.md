# Calculate the mean error

Calculate the mean error of a numeric vector.

## Usage

``` r
me_vec(truth, estimate, na_rm = TRUE)
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

`me_vec()` computes the average signed forecast error
`truth - estimate`. Positive values indicate that forecasts are, on
average, below the observed values. Negative values indicate that
forecasts are, on average, above the observed values.

The metric is reported in the same units as the original data.

## See also

Other accuracy functions:
[`mae_vec()`](https://ahaeusser.github.io/tscv/reference/mae_vec.md),
[`make_accuracy()`](https://ahaeusser.github.io/tscv/reference/make_accuracy.md),
[`make_errors()`](https://ahaeusser.github.io/tscv/reference/make_errors.md),
[`mape_vec()`](https://ahaeusser.github.io/tscv/reference/mape_vec.md),
[`mpe_vec()`](https://ahaeusser.github.io/tscv/reference/mpe_vec.md),
[`mse_vec()`](https://ahaeusser.github.io/tscv/reference/mse_vec.md),
[`rmse_vec()`](https://ahaeusser.github.io/tscv/reference/rmse_vec.md),
[`smape_vec()`](https://ahaeusser.github.io/tscv/reference/smape_vec.md)

## Examples

``` r
truth <- c(10, 20, 30)
estimate <- c(8, 22, 25)

me_vec(truth, estimate)
#> [1] 1.666667

truth_na <- c(10, 20, NA)
estimate_na <- c(8, 22, 25)
me_vec(truth_na, estimate_na)
#> [1] 0
```
