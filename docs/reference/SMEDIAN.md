# Seasonal median model

Specify a seasonal median benchmark model for use with
[`fabletools::model()`](https://fabletools.tidyverts.org/reference/model.html).

## Usage

``` r
SMEDIAN(formula, ...)
```

## Arguments

- formula:

  A model formula specifying the response and
  [`lag()`](https://dplyr.tidyverse.org/reference/lead-lag.html)
  special, for example `value ~ lag("week")`.

- ...:

  Further arguments.

## Value

A model definition that can be used inside
[`fabletools::model()`](https://fabletools.tidyverts.org/reference/model.html).

## Details

`SMEDIAN()` forecasts each future observation using the historical
median of the matching seasonal position. Use the
[`lag()`](https://dplyr.tidyverse.org/reference/lead-lag.html) special
to define the seasonal period, for example `lag("week")` for hourly data
with weekly seasonality.

## See also

Other SMEDIAN:
[`fitted.SMEDIAN()`](https://ahaeusser.github.io/tscv/reference/fitted.SMEDIAN.md),
[`forecast.SMEDIAN()`](https://ahaeusser.github.io/tscv/reference/forecast.SMEDIAN.md),
[`model_sum.SMEDIAN()`](https://ahaeusser.github.io/tscv/reference/model_sum.SMEDIAN.md),
[`residuals.SMEDIAN()`](https://ahaeusser.github.io/tscv/reference/residuals.SMEDIAN.md)

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
  model("SMEDIAN" = SMEDIAN(value ~ lag("week")))

model_frame
#> # A mable: 1 x 1
#>     SMEDIAN
#>     <model>
#> 1 <SMEDIAN>
```
