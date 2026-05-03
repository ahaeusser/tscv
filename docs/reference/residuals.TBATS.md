# Extract residuals from a TBATS model

Extract residuals from a fitted `TBATS` model.

## Usage

``` r
# S3 method for class 'TBATS'
residuals(object, ...)
```

## Arguments

- object:

  A fitted `TBATS` model object.

- ...:

  Additional arguments. Currently not used.

## Value

Residuals.

## Details

This method is used by
[`residuals()`](https://rdrr.io/r/stats/residuals.html) when extracting
residuals from a mable containing a `TBATS` model.

## See also

Other TBATS:
[`TBATS()`](https://ahaeusser.github.io/tscv/reference/TBATS.md),
[`fitted.TBATS()`](https://ahaeusser.github.io/tscv/reference/fitted.TBATS.md),
[`forecast.TBATS()`](https://ahaeusser.github.io/tscv/reference/forecast.TBATS.md),
[`model_sum.TBATS()`](https://ahaeusser.github.io/tscv/reference/model_sum.TBATS.md)

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

residuals(model_frame)
#> # A tsibble: 504 x 3 [1h] <UTC>
#> # Key:       .model [1]
#>    .model time                 .resid
#>    <chr>  <dttm>                <dbl>
#>  1 TBATS  2019-01-01 00:00:00  15.0  
#>  2 TBATS  2019-01-01 01:00:00 -13.9  
#>  3 TBATS  2019-01-01 02:00:00  -4.88 
#>  4 TBATS  2019-01-01 03:00:00   2.25 
#>  5 TBATS  2019-01-01 04:00:00  -7.75 
#>  6 TBATS  2019-01-01 05:00:00 -12.0  
#>  7 TBATS  2019-01-01 06:00:00  -7.08 
#>  8 TBATS  2019-01-01 07:00:00   5.14 
#>  9 TBATS  2019-01-01 08:00:00  -2.88 
#> 10 TBATS  2019-01-01 09:00:00   0.107
#> # ℹ 494 more rows
# }
```
