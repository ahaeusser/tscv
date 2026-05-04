# Calculate the symmetric mean absolute percentage error

Calculate the symmetric mean absolute percentage error of a numeric
vector.

## Usage

``` r
smape_vec(truth, estimate, na_rm = TRUE)
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

`smape_vec()` computes the symmetric mean absolute percentage error:
`abs(estimate - truth) / ((abs(truth) + abs(estimate)) / 2) * 100`.

This metric is undefined when both `truth` and `estimate` are zero and
may return `NaN` in such cases.

## See also

Other accuracy functions:
[`mae_vec()`](https://ahaeusser.github.io/tscv/reference/mae_vec.md),
[`make_accuracy()`](https://ahaeusser.github.io/tscv/reference/make_accuracy.md),
[`make_errors()`](https://ahaeusser.github.io/tscv/reference/make_errors.md),
[`mape_vec()`](https://ahaeusser.github.io/tscv/reference/mape_vec.md),
[`me_vec()`](https://ahaeusser.github.io/tscv/reference/me_vec.md),
[`mpe_vec()`](https://ahaeusser.github.io/tscv/reference/mpe_vec.md),
[`mse_vec()`](https://ahaeusser.github.io/tscv/reference/mse_vec.md),
[`rmse_vec()`](https://ahaeusser.github.io/tscv/reference/rmse_vec.md)

## Examples

``` r
truth <- c(10, 20, 40)
estimate <- c(8, 22, 30)

smape_vec(truth, estimate)
#> [1] 20.10582

truth_na <- c(10, 20, NA)
estimate_na <- c(8, 22, 25)
smape_vec(truth_na, estimate_na)
#> [1] 15.87302
```
