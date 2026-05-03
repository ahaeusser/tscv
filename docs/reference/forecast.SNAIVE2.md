# Forecast a SNAIVE2 model

Forecast a fitted `SNAIVE2` model.

## Usage

``` r
# S3 method for class 'SNAIVE2'
forecast(object, new_data, specials = NULL, ...)
```

## Arguments

- object:

  A fitted `SNAIVE2` model object.

- new_data:

  A `tsibble` containing future time points.

- specials:

  Parsed specials. Currently not used.

- ...:

  Additional arguments. Currently not used.

## Value

A vector of forecast distributions.

## See also

Other SNAIVE2:
[`SNAIVE2()`](https://ahaeusser.github.io/tscv/reference/SNAIVE2.md),
[`fitted.SNAIVE2()`](https://ahaeusser.github.io/tscv/reference/fitted.SNAIVE2.md),
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

forecast(model_frame, h = 24)
#> # A fable: 24 x 4 [1h] <UTC>
#> # Key:     .model [1]
#>    .model  time               
#>    <chr>   <dttm>             
#>  1 SNAIVE2 2019-01-22 00:00:00
#>  2 SNAIVE2 2019-01-22 01:00:00
#>  3 SNAIVE2 2019-01-22 02:00:00
#>  4 SNAIVE2 2019-01-22 03:00:00
#>  5 SNAIVE2 2019-01-22 04:00:00
#>  6 SNAIVE2 2019-01-22 05:00:00
#>  7 SNAIVE2 2019-01-22 06:00:00
#>  8 SNAIVE2 2019-01-22 07:00:00
#>  9 SNAIVE2 2019-01-22 08:00:00
#> 10 SNAIVE2 2019-01-22 09:00:00
#> # ℹ 14 more rows
#> # ℹ 2 more variables: value <dist>, .mean <dbl>
```
