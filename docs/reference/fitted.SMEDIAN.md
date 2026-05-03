# Extract fitted values from a seasonal median model

Extract fitted values from a fitted `SMEDIAN` model.

## Usage

``` r
# S3 method for class 'SMEDIAN'
fitted(object, ...)
```

## Arguments

- object:

  A fitted `SMEDIAN` model object.

- ...:

  Additional arguments. Currently not used.

## Value

Fitted values.

## See also

Other SMEDIAN:
[`SMEDIAN()`](https://ahaeusser.github.io/tscv/reference/SMEDIAN.md),
[`forecast.SMEDIAN()`](https://ahaeusser.github.io/tscv/reference/forecast.SMEDIAN.md),
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

fitted(model_frame)
#> # A tsibble: 504 x 3 [1h] <UTC>
#> # Key:       .model [1]
#>    .model  time                .fitted
#>    <chr>   <dttm>                <dbl>
#>  1 SMEDIAN 2019-01-01 00:00:00   20.9 
#>  2 SMEDIAN 2019-01-01 01:00:00    7.78
#>  3 SMEDIAN 2019-01-01 02:00:00   14.3 
#>  4 SMEDIAN 2019-01-01 03:00:00   18.6 
#>  5 SMEDIAN 2019-01-01 04:00:00   18.6 
#>  6 SMEDIAN 2019-01-01 05:00:00   35.8 
#>  7 SMEDIAN 2019-01-01 06:00:00   43.9 
#>  8 SMEDIAN 2019-01-01 07:00:00   46.9 
#>  9 SMEDIAN 2019-01-01 08:00:00   43.9 
#> 10 SMEDIAN 2019-01-01 09:00:00   43.8 
#> # ℹ 494 more rows
```
