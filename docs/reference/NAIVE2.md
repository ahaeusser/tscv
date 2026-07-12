# Naive2 model

Specify a Naive2 benchmark model for use with
[`fabletools::model()`](https://fabletools.tidyverts.org/reference/model.html).

## Usage

``` r
NAIVE2(formula, ...)
```

## Arguments

- formula:

  A model formula specifying the response and optional `season()`
  special, for example `value ~ season(12)`.

- ...:

  Further arguments.

## Value

A model definition that can be used inside
[`fabletools::model()`](https://fabletools.tidyverts.org/reference/model.html).

## Details

`NAIVE2()` implements the Naive2 benchmark from the M4 forecasting
competition. The method tests the response for seasonality at the
specified frequency.

If seasonality is detected, the response is adjusted using classical
multiplicative decomposition. Naive forecasts are produced from the
seasonally adjusted response and subsequently reseasonalized.

If seasonality is not detected, the method is equivalent to an ordinary
naive forecast.

The `season()` special controls the seasonal frequency. When
`period = NULL`, the frequency is inferred from the tsibble index.
Alternatively, it can be specified explicitly, such as `season(12)` for
monthly data.

## See also

Other NAIVE2:
[`fitted.NAIVE2()`](https://ahaeusser.github.io/tscv/reference/fitted.NAIVE2.md),
[`forecast.NAIVE2()`](https://ahaeusser.github.io/tscv/reference/forecast.NAIVE2.md),
[`model_sum.NAIVE2()`](https://ahaeusser.github.io/tscv/reference/model_sum.NAIVE2.md),
[`residuals.NAIVE2()`](https://ahaeusser.github.io/tscv/reference/residuals.NAIVE2.md)

## Examples

``` r
library(dplyr)
library(tsibble)
library(fabletools)

train_frame <- M4_monthly_data |>
  filter(series == first(series)) |>
  as_tsibble(index = index)

model_frame <- train_frame |>
  model("NAIVE2" = NAIVE2(value ~ season(12)))

model_frame
#> # A mable: 1 x 1
#>     NAIVE2
#>    <model>
#> 1 <NAIVE2>
```
