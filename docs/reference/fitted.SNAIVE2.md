# Extract fitted values from a SNAIVE2 model

Extract fitted values from a fitted `SNAIVE2` model.

## Usage

``` r
# S3 method for class 'SNAIVE2'
fitted(object, ...)
```

## Arguments

- object:

  A fitted `SNAIVE2` model object.

- ...:

  Additional arguments. Currently not used.

## Value

Fitted values.

## See also

Other SNAIVE2:
[`SNAIVE2()`](https://ahaeusser.github.io/tscv/reference/SNAIVE2.md),
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

fitted(model_frame)
#> # A tsibble: 504 x 3 [1h] <UTC>
#> # Key:       .model [1]
#>    .model  time                .fitted
#>    <chr>   <dttm>                <dbl>
#>  1 SNAIVE2 2019-01-01 00:00:00      NA
#>  2 SNAIVE2 2019-01-01 01:00:00      NA
#>  3 SNAIVE2 2019-01-01 02:00:00      NA
#>  4 SNAIVE2 2019-01-01 03:00:00      NA
#>  5 SNAIVE2 2019-01-01 04:00:00      NA
#>  6 SNAIVE2 2019-01-01 05:00:00      NA
#>  7 SNAIVE2 2019-01-01 06:00:00      NA
#>  8 SNAIVE2 2019-01-01 07:00:00      NA
#>  9 SNAIVE2 2019-01-01 08:00:00      NA
#> 10 SNAIVE2 2019-01-01 09:00:00      NA
#> # ℹ 494 more rows
```
