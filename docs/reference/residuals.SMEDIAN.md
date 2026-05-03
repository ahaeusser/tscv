# Extract residuals from a seasonal median model

Extract residuals from a fitted `SMEDIAN` model.

## Usage

``` r
# S3 method for class 'SMEDIAN'
residuals(object, ...)
```

## Arguments

- object:

  A fitted `SMEDIAN` model object.

- ...:

  Additional arguments. Currently not used.

## Value

Residuals.

## See also

Other SMEDIAN:
[`SMEDIAN()`](https://ahaeusser.github.io/tscv/reference/SMEDIAN.md),
[`fitted.SMEDIAN()`](https://ahaeusser.github.io/tscv/reference/fitted.SMEDIAN.md),
[`forecast.SMEDIAN()`](https://ahaeusser.github.io/tscv/reference/forecast.SMEDIAN.md),
[`model_sum.SMEDIAN()`](https://ahaeusser.github.io/tscv/reference/model_sum.SMEDIAN.md)

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

residuals(model_frame)
#> # A tsibble: 504 x 3 [1h] <UTC>
#> # Key:       .model [1]
#>    .model  time                .resid
#>    <chr>   <dttm>               <dbl>
#>  1 SMEDIAN 2019-01-01 00:00:00  -10.8
#>  2 SMEDIAN 2019-01-01 01:00:00  -11.9
#>  3 SMEDIAN 2019-01-01 02:00:00  -24.2
#>  4 SMEDIAN 2019-01-01 03:00:00  -26.0
#>  5 SMEDIAN 2019-01-01 04:00:00  -31.1
#>  6 SMEDIAN 2019-01-01 05:00:00  -53.1
#>  7 SMEDIAN 2019-01-01 06:00:00  -58.9
#>  8 SMEDIAN 2019-01-01 07:00:00  -51.9
#>  9 SMEDIAN 2019-01-01 08:00:00  -50.2
#> 10 SMEDIAN 2019-01-01 09:00:00  -48.8
#> # ℹ 494 more rows
```
