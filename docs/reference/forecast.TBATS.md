# Forecast a TBATS model

Forecast a fitted `TBATS` model.

## Usage

``` r
# S3 method for class 'TBATS'
forecast(object, new_data, specials = NULL, ...)
```

## Arguments

- object:

  A fitted `TBATS` model object.

- new_data:

  A `tsibble` containing future time points.

- specials:

  Parsed specials. Currently not used.

- ...:

  Additional arguments. Currently not used.

## Value

A vector of forecast distributions.

## Details

This method is used by
[`forecast()`](https://generics.r-lib.org/reference/forecast.html) when
forecasting a mable containing a `TBATS` model.

## See also

Other TBATS:
[`TBATS()`](https://ahaeusser.github.io/tscv/reference/TBATS.md),
[`fitted.TBATS()`](https://ahaeusser.github.io/tscv/reference/fitted.TBATS.md),
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

forecast(model_frame, h = 24)
#> # A fable: 24 x 4 [1h] <UTC>
#> # Key:     .model [1]
#>    .model time               
#>    <chr>  <dttm>             
#>  1 TBATS  2019-01-22 00:00:00
#>  2 TBATS  2019-01-22 01:00:00
#>  3 TBATS  2019-01-22 02:00:00
#>  4 TBATS  2019-01-22 03:00:00
#>  5 TBATS  2019-01-22 04:00:00
#>  6 TBATS  2019-01-22 05:00:00
#>  7 TBATS  2019-01-22 06:00:00
#>  8 TBATS  2019-01-22 07:00:00
#>  9 TBATS  2019-01-22 08:00:00
#> 10 TBATS  2019-01-22 09:00:00
#> # ℹ 14 more rows
#> # ℹ 2 more variables: value <dist>, .mean <dbl>
# }
```
