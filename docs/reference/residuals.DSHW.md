# Extract residuals from a DSHW model

Extract residuals from a fitted `DSHW` model.

## Usage

``` r
# S3 method for class 'DSHW'
residuals(object, ...)
```

## Arguments

- object:

  A fitted `DSHW` model object.

- ...:

  Additional arguments. Currently not used.

## Value

Residuals.

## See also

Other DSHW:
[`DSHW()`](https://ahaeusser.github.io/tscv/reference/DSHW.md),
[`fitted.DSHW()`](https://ahaeusser.github.io/tscv/reference/fitted.DSHW.md),
[`forecast.DSHW()`](https://ahaeusser.github.io/tscv/reference/forecast.DSHW.md),
[`model_sum.DSHW()`](https://ahaeusser.github.io/tscv/reference/model_sum.DSHW.md)

## Examples

``` r
# \donttest{
library(dplyr)
library(tsibble)
library(fabletools)

train_frame <- elec_price |>
  filter(bidding_zone == "DE") |>
  slice_head(n = 24 * 28) |>
  as_tsibble(index = time)

model_frame <- train_frame |>
  model("DSHW" = DSHW(value, periods = c(24, 168)))
#> Warning: 1 error encountered for DSHW
#> [1] dshw not suitable when data contain zeros or negative numbers

residuals(model_frame)
#> # A tsibble: 672 x 3 [1h] <UTC>
#> # Key:       .model [1]
#>    .model time                .resid
#>    <chr>  <dttm>               <dbl>
#>  1 DSHW   2019-01-01 00:00:00     NA
#>  2 DSHW   2019-01-01 01:00:00     NA
#>  3 DSHW   2019-01-01 02:00:00     NA
#>  4 DSHW   2019-01-01 03:00:00     NA
#>  5 DSHW   2019-01-01 04:00:00     NA
#>  6 DSHW   2019-01-01 05:00:00     NA
#>  7 DSHW   2019-01-01 06:00:00     NA
#>  8 DSHW   2019-01-01 07:00:00     NA
#>  9 DSHW   2019-01-01 08:00:00     NA
#> 10 DSHW   2019-01-01 09:00:00     NA
#> # ℹ 662 more rows
# }
```
