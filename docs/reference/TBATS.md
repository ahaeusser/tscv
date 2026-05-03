# TBATS model

Specify a TBATS model for use with
[`fabletools::model()`](https://fabletools.tidyverts.org/reference/model.html).

## Usage

``` r
TBATS(formula, ...)
```

## Arguments

- formula:

  A model formula specifying the response variable, for example `value`.

- ...:

  Further arguments passed to
  [`forecast::tbats()`](https://pkg.robjhyndman.com/forecast/reference/tbats.html),
  including `periods`.

## Value

A model definition that can be used inside
[`fabletools::model()`](https://fabletools.tidyverts.org/reference/model.html).

## Details

`TBATS()` is a model specification wrapper around
[`forecast::tbats()`](https://pkg.robjhyndman.com/forecast/reference/tbats.html)
for the `fable`, `tsibble`, and `fabletools` ecosystem.

TBATS stands for trigonometric seasonality, Box-Cox transformation, ARMA
errors, trend, and seasonal components. It can be useful for time series
with multiple or complex seasonal patterns.

The seasonal periods must be supplied through the `periods` argument.

## See also

Other TBATS:
[`fitted.TBATS()`](https://ahaeusser.github.io/tscv/reference/fitted.TBATS.md),
[`forecast.TBATS()`](https://ahaeusser.github.io/tscv/reference/forecast.TBATS.md),
[`model_sum.TBATS()`](https://ahaeusser.github.io/tscv/reference/model_sum.TBATS.md),
[`residuals.TBATS()`](https://ahaeusser.github.io/tscv/reference/residuals.TBATS.md)

## Examples

``` r
# \donttest{
library(dplyr)
library(tsibble)
library(fabletools)

train_frame <- elec_price |>
  filter(bidding_zone == "DE") |>
  slice_head(n = 24 * 21) |>
  as_tsibble(index = time)

model_frame <- train_frame |>
  model("TBATS" = TBATS(value, periods = c(24, 168)))

model_frame
#> # A mable: 1 x 1
#>     TBATS
#>   <model>
#> 1 <TBATS>
# }
```
