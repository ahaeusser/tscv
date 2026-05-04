# Calculate the mean absolute error

Calculate the mean absolute error of a numeric vector.

## Usage

``` r
mae_vec(truth, estimate, na_rm = TRUE)
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

`mae_vec()` computes the average absolute forecast error
`abs(truth - estimate)`. The metric is reported in the same units as the
original data.

## See also

Other accuracy functions:
[`make_accuracy()`](https://ahaeusser.github.io/tscv/reference/make_accuracy.md),
[`make_errors()`](https://ahaeusser.github.io/tscv/reference/make_errors.md),
[`mape_vec()`](https://ahaeusser.github.io/tscv/reference/mape_vec.md),
[`me_vec()`](https://ahaeusser.github.io/tscv/reference/me_vec.md),
[`mpe_vec()`](https://ahaeusser.github.io/tscv/reference/mpe_vec.md),
[`mse_vec()`](https://ahaeusser.github.io/tscv/reference/mse_vec.md),
[`rmse_vec()`](https://ahaeusser.github.io/tscv/reference/rmse_vec.md),
[`smape_vec()`](https://ahaeusser.github.io/tscv/reference/smape_vec.md)

## Examples

``` r
truth <- c(10, 20, 30)
estimate <- c(8, 22, 25)

mae_vec(truth, estimate)
#> [1] 3

truth_na <- c(10, 20, NA)
estimate_na <- c(8, 22, 25)
mae_vec(truth_na, estimate_na)
#> [1] 2
```
