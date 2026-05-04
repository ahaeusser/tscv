# Double Seasonal Holt-Winters model

Specify a Double Seasonal Holt-Winters model for use with
[`fabletools::model()`](https://fabletools.tidyverts.org/reference/model.html).

## Usage

``` r
DSHW(formula, ...)
```

## Arguments

- formula:

  A model formula specifying the response variable, for example `value`.

- ...:

  Further arguments passed to
  [`forecast::dshw()`](https://pkg.robjhyndman.com/forecast/reference/dshw.html),
  including `periods`.

## Value

A model definition that can be used inside
[`fabletools::model()`](https://fabletools.tidyverts.org/reference/model.html).

## Details

`DSHW()` is a model specification wrapper around
[`forecast::dshw()`](https://pkg.robjhyndman.com/forecast/reference/dshw.html)
for the `fable`, `tsibble`, and `fabletools` ecosystem.

The model is useful for time series with two important seasonal
patterns, such as hourly data with daily and weekly seasonality.

The seasonal periods must be supplied through the `periods` argument.

## See also

Other DSHW:
[`fitted.DSHW()`](https://ahaeusser.github.io/tscv/reference/fitted.DSHW.md),
[`forecast.DSHW()`](https://ahaeusser.github.io/tscv/reference/forecast.DSHW.md),
[`model_sum.DSHW()`](https://ahaeusser.github.io/tscv/reference/model_sum.DSHW.md),
[`residuals.DSHW()`](https://ahaeusser.github.io/tscv/reference/residuals.DSHW.md)

## Examples

``` r
# \donttest{
library(dplyr)
library(tsibble)
library(fabletools)

train_frame <- elec_load |>
  filter(bidding_zone == "DE") |>
  slice_head(n = 24 * 28) |>
  as_tsibble(index = time)

model_frame <- train_frame |>
  model("DSHW" = DSHW(value, periods = c(24, 168)))
#> Warning: 1 error encountered for DSHW
#> [1] DSHW does not support missing values.

model_frame
#> # A mable: 1 x 1
#>           DSHW
#>        <model>
#> 1 <NULL model>
# }
```
