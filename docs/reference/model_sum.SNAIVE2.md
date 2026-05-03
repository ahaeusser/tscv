# Summarize a SNAIVE2 model

Return a short model label for a fitted `SNAIVE2` model.

## Usage

``` r
# S3 method for class 'SNAIVE2'
model_sum(x)
```

## Arguments

- x:

  A fitted `SNAIVE2` model object.

## Value

A character string.

## See also

Other SNAIVE2:
[`SNAIVE2()`](https://ahaeusser.github.io/tscv/reference/SNAIVE2.md),
[`fitted.SNAIVE2()`](https://ahaeusser.github.io/tscv/reference/fitted.SNAIVE2.md),
[`forecast.SNAIVE2()`](https://ahaeusser.github.io/tscv/reference/forecast.SNAIVE2.md),
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

model_frame
#> # A mable: 1 x 1
#>     SNAIVE2
#>     <model>
#> 1 <SNAIVE2>
```
