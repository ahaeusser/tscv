# Calculate the mean absolute percentage error

Calculate the mean absolute percentage error of a numeric vector.

## Usage

``` r
mape_vec(truth, estimate, na_rm = TRUE)
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

`mape_vec()` computes the average absolute percentage forecast error:
`abs((truth - estimate) / truth) * 100`.

This metric is undefined when `truth` is zero and may return `Inf` or
`NaN` in such cases.

## See also

Other accuracy functions:
[`mae_vec()`](https://ahaeusser.github.io/tscv/reference/mae_vec.md),
[`make_accuracy()`](https://ahaeusser.github.io/tscv/reference/make_accuracy.md),
[`make_errors()`](https://ahaeusser.github.io/tscv/reference/make_errors.md),
[`me_vec()`](https://ahaeusser.github.io/tscv/reference/me_vec.md),
[`mpe_vec()`](https://ahaeusser.github.io/tscv/reference/mpe_vec.md),
[`mse_vec()`](https://ahaeusser.github.io/tscv/reference/mse_vec.md),
[`rmse_vec()`](https://ahaeusser.github.io/tscv/reference/rmse_vec.md),
[`smape_vec()`](https://ahaeusser.github.io/tscv/reference/smape_vec.md)

## Examples

``` r
truth <- c(10, 20, 40)
estimate <- c(8, 22, 30)

mape_vec(truth, estimate)
#> [1] 18.33333

truth_na <- c(10, 20, NA)
estimate_na <- c(8, 22, 25)
mape_vec(truth_na, estimate_na)
#> [1] 15
```
