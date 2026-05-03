# Extract fitted values from a TBATS model

Extract fitted values from a fitted `TBATS` model.

## Usage

``` r
# S3 method for class 'TBATS'
fitted(object, ...)
```

## Arguments

- object:

  A fitted `TBATS` model object.

- ...:

  Additional arguments. Currently not used.

## Value

Fitted values.

## Details

This method is used by
[`fitted()`](https://rdrr.io/r/stats/fitted.values.html) when extracting
fitted values from a mable containing a `TBATS` model.

## See also

Other TBATS:
[`TBATS()`](https://ahaeusser.github.io/tscv/reference/TBATS.md),
[`forecast.TBATS()`](https://ahaeusser.github.io/tscv/reference/forecast.TBATS.md),
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

fitted(model_frame)
#> # A tsibble: 504 x 3 [1h] <UTC>
#> # Key:       .model [1]
#>    .model time                .fitted
#>    <chr>  <dttm>                <dbl>
#>  1 TBATS  2019-01-01 00:00:00   -4.93
#>  2 TBATS  2019-01-01 01:00:00    9.81
#>  3 TBATS  2019-01-01 02:00:00   -5.03
#>  4 TBATS  2019-01-01 03:00:00   -9.66
#>  5 TBATS  2019-01-01 04:00:00   -4.80
#>  6 TBATS  2019-01-01 05:00:00   -5.21
#>  7 TBATS  2019-01-01 06:00:00   -7.99
#>  8 TBATS  2019-01-01 07:00:00  -10.1 
#>  9 TBATS  2019-01-01 08:00:00   -3.45
#> 10 TBATS  2019-01-01 09:00:00   -5.04
#> # ℹ 494 more rows
# }
```
