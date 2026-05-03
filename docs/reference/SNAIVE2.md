# Seasonal naive model with weekday-specific lags

Specify a seasonal naive benchmark model for use with
[`fabletools::model()`](https://fabletools.tidyverts.org/reference/model.html).

## Usage

``` r
SNAIVE2(formula, ...)
```

## Arguments

- formula:

  A model formula specifying the response variable, for example `value`.

- ...:

  Further arguments.

## Value

A model definition that can be used inside
[`fabletools::model()`](https://fabletools.tidyverts.org/reference/model.html).

## Details

`SNAIVE2()` is intended for hourly time series. It uses a daily lag for
Tuesday to Friday observations and a weekly lag otherwise. This can be
useful for electricity price or load data where weekdays have similar
intraday structure and weekends require a weekly comparison.

## See also

Other SNAIVE2:
[`fitted.SNAIVE2()`](https://ahaeusser.github.io/tscv/reference/fitted.SNAIVE2.md),
[`forecast.SNAIVE2()`](https://ahaeusser.github.io/tscv/reference/forecast.SNAIVE2.md),
[`model_sum.SNAIVE2()`](https://ahaeusser.github.io/tscv/reference/model_sum.SNAIVE2.md),
[`residuals.SNAIVE2()`](https://ahaeusser.github.io/tscv/reference/residuals.SNAIVE2.md)

## Examples

``` r
library(dplyr)
library(tsibble)
library(fabletools)

train_frame <- elec_price |>
  filter(bidding_zone == "DE") |>
  slice_head(n = 24 * 21) |>
  as_tsibble(index = time)

model_frame <- train_frame |>
  model("SNAIVE2" = SNAIVE2(value))

model_frame
#> # A mable: 1 x 1
#>     SNAIVE2
#>     <model>
#> 1 <SNAIVE2>
```
