# Summarize a DSHW model

Return a short model label for a fitted `DSHW` model.

## Usage

``` r
# S3 method for class 'DSHW'
model_sum(x)
```

## Arguments

- x:

  A fitted `DSHW` model object.

## Value

A character string.

## See also

Other DSHW:
[`DSHW()`](https://ahaeusser.github.io/tscv/reference/DSHW.md),
[`fitted.DSHW()`](https://ahaeusser.github.io/tscv/reference/fitted.DSHW.md),
[`forecast.DSHW()`](https://ahaeusser.github.io/tscv/reference/forecast.DSHW.md),
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

model_frame
#> # A mable: 1 x 1
#>           DSHW
#>        <model>
#> 1 <NULL model>
# }
```
