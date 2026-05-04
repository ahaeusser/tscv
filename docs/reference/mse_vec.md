# Calculate the mean squared error

Calculate the mean squared error of a numeric vector.

## Usage

``` r
mse_vec(truth, estimate, na_rm = TRUE)
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

`mse_vec()` computes the average squared forecast error
`(truth - estimate)^2`. The metric is reported in squared units of the
original data.

## See also

Other accuracy functions:
[`mae_vec()`](https://ahaeusser.github.io/tscv/reference/mae_vec.md),
[`make_accuracy()`](https://ahaeusser.github.io/tscv/reference/make_accuracy.md),
[`make_errors()`](https://ahaeusser.github.io/tscv/reference/make_errors.md),
[`mape_vec()`](https://ahaeusser.github.io/tscv/reference/mape_vec.md),
[`me_vec()`](https://ahaeusser.github.io/tscv/reference/me_vec.md),
[`mpe_vec()`](https://ahaeusser.github.io/tscv/reference/mpe_vec.md),
[`rmse_vec()`](https://ahaeusser.github.io/tscv/reference/rmse_vec.md),
[`smape_vec()`](https://ahaeusser.github.io/tscv/reference/smape_vec.md)

## Examples

``` r
truth <- c(10, 20, 30)
estimate <- c(8, 22, 25)

mse_vec(truth, estimate)
#> [1] 11

truth_na <- c(10, 20, NA)
estimate_na <- c(8, 22, 25)
mse_vec(truth_na, estimate_na)
#> [1] 4
```
