# Seasonal mean model

Specify a seasonal mean benchmark model for use with
[`fabletools::model()`](https://fabletools.tidyverts.org/reference/model.html).

## Usage

``` r
SMEAN(formula, ...)
```

## Arguments

- formula:

  A model formula specifying the response and
  [`lag()`](https://dplyr.tidyverse.org/reference/lead-lag.html)
  special, for example `value ~ lag("year")`.

- ...:

  Further arguments.

## Value

A model definition that can be used inside
[`fabletools::model()`](https://fabletools.tidyverts.org/reference/model.html).

## Details

`SMEAN()` forecasts each future observation using the historical mean of
the matching seasonal position. Use the
[`lag()`](https://dplyr.tidyverse.org/reference/lead-lag.html) special
to define the seasonal period, for example `lag("year")` for monthly
data.

## See also

Other SMEAN:
[`fitted.SMEAN()`](https://ahaeusser.github.io/tscv/reference/fitted.SMEAN.md),
[`forecast.SMEAN()`](https://ahaeusser.github.io/tscv/reference/forecast.SMEAN.md),
[`model_sum.SMEAN()`](https://ahaeusser.github.io/tscv/reference/model_sum.SMEAN.md),
[`residuals.SMEAN()`](https://ahaeusser.github.io/tscv/reference/residuals.SMEAN.md)

## Examples

``` r
library(dplyr)
library(tsibble)
library(fabletools)

train_frame <- M4_monthly_data |>
  filter(series == first(series)) |>
  as_tsibble(index = index)

model_frame <- train_frame |>
  model("SMEAN" = SMEAN(value ~ lag("year")))

model_frame
#> # A mable: 1 x 1
#>     SMEAN
#>   <model>
#> 1 <SMEAN>
```
