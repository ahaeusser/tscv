# Median model

Specify a median benchmark model for use with
[`fabletools::model()`](https://fabletools.tidyverts.org/reference/model.html).

## Usage

``` r
MEDIAN(formula, ...)
```

## Arguments

- formula:

  A model formula specifying the response and optional
  [`window()`](https://rdrr.io/r/stats/window.html) special, for example
  `value ~ window()`.

- ...:

  Further arguments.

## Value

A model definition that can be used inside
[`fabletools::model()`](https://fabletools.tidyverts.org/reference/model.html).

## Details

`MEDIAN()` forecasts future values using the median of the observed
response. The [`window()`](https://rdrr.io/r/stats/window.html) special
controls whether the median is estimated using all observations, a fixed
trailing window, or a rolling window.

## See also

Other MEDIAN:
[`fitted.MEDIAN()`](https://ahaeusser.github.io/tscv/reference/fitted.MEDIAN.md),
[`forecast.MEDIAN()`](https://ahaeusser.github.io/tscv/reference/forecast.MEDIAN.md),
[`model_sum.MEDIAN()`](https://ahaeusser.github.io/tscv/reference/model_sum.MEDIAN.md),
[`residuals.MEDIAN()`](https://ahaeusser.github.io/tscv/reference/residuals.MEDIAN.md)

## Examples

``` r
library(dplyr)
library(tsibble)
library(fabletools)

train_frame <- M4_monthly_data |>
  filter(series == first(series)) |>
  as_tsibble(index = index)

model_frame <- train_frame |>
  model("MEDIAN" = MEDIAN(value ~ window()))

model_frame
#> # A mable: 1 x 1
#>     MEDIAN
#>    <model>
#> 1 <MEDIAN>
```
