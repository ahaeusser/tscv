# Forecast a seasonal median model

Forecast a fitted `SMEDIAN` model.

## Usage

``` r
# S3 method for class 'SMEDIAN'
forecast(object, new_data, specials = NULL, ...)
```

## Arguments

- object:

  A fitted `SMEDIAN` model object.

- new_data:

  A `tsibble` containing future time points.

- specials:

  Parsed specials. Currently not used.

- ...:

  Additional arguments. Currently not used.

## Value

A vector of forecast distributions.

## See also

Other SMEDIAN:
[`SMEDIAN()`](https://ahaeusser.github.io/tscv/reference/SMEDIAN.md),
[`fitted.SMEDIAN()`](https://ahaeusser.github.io/tscv/reference/fitted.SMEDIAN.md),
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

forecast(model_frame, h = 24)
#> # A fable: 24 x 4 [1h] <UTC>
#> # Key:     .model [1]
#>    .model  time               
#>    <chr>   <dttm>             
#>  1 SMEDIAN 2019-01-22 00:00:00
#>  2 SMEDIAN 2019-01-22 01:00:00
#>  3 SMEDIAN 2019-01-22 02:00:00
#>  4 SMEDIAN 2019-01-22 03:00:00
#>  5 SMEDIAN 2019-01-22 04:00:00
#>  6 SMEDIAN 2019-01-22 05:00:00
#>  7 SMEDIAN 2019-01-22 06:00:00
#>  8 SMEDIAN 2019-01-22 07:00:00
#>  9 SMEDIAN 2019-01-22 08:00:00
#> 10 SMEDIAN 2019-01-22 09:00:00
#> # ℹ 14 more rows
#> # ℹ 2 more variables: value <dist>, .mean <dbl>
```
