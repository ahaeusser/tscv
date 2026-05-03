# Forecast a DSHW model

Forecast a fitted `DSHW` model.

## Usage

``` r
# S3 method for class 'DSHW'
forecast(object, new_data, specials = NULL, ...)
```

## Arguments

- object:

  A fitted `DSHW` model object.

- new_data:

  A `tsibble` containing future time points.

- specials:

  Parsed specials. Currently not used.

- ...:

  Additional arguments. Currently not used.

## Value

A vector of forecast distributions.

## See also

Other DSHW:
[`DSHW()`](https://ahaeusser.github.io/tscv/reference/DSHW.md),
[`fitted.DSHW()`](https://ahaeusser.github.io/tscv/reference/fitted.DSHW.md),
[`model_sum.DSHW()`](https://ahaeusser.github.io/tscv/reference/model_sum.DSHW.md),
[`residuals.DSHW()`](https://ahaeusser.github.io/tscv/reference/residuals.DSHW.md)

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

forecast(model_frame, h = 24)
#> # A fable: 24 x 4 [1h] <UTC>
#> # Key:     .model [1]
#>    .model time                 value .mean
#>    <chr>  <dttm>              <dist> <dbl>
#>  1 DSHW   2019-01-29 00:00:00     NA    NA
#>  2 DSHW   2019-01-29 01:00:00     NA    NA
#>  3 DSHW   2019-01-29 02:00:00     NA    NA
#>  4 DSHW   2019-01-29 03:00:00     NA    NA
#>  5 DSHW   2019-01-29 04:00:00     NA    NA
#>  6 DSHW   2019-01-29 05:00:00     NA    NA
#>  7 DSHW   2019-01-29 06:00:00     NA    NA
#>  8 DSHW   2019-01-29 07:00:00     NA    NA
#>  9 DSHW   2019-01-29 08:00:00     NA    NA
#> 10 DSHW   2019-01-29 09:00:00     NA    NA
#> # ℹ 14 more rows
# }
```
