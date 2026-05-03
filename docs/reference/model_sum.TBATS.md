# Summarize a TBATS model

Return a short model label for a fitted `TBATS` model.

## Usage

``` r
# S3 method for class 'TBATS'
model_sum(x)
```

## Arguments

- x:

  A fitted `TBATS` model object.

## Value

A character string.

## See also

Other TBATS:
[`TBATS()`](https://ahaeusser.github.io/tscv/reference/TBATS.md),
[`fitted.TBATS()`](https://ahaeusser.github.io/tscv/reference/fitted.TBATS.md),
[`forecast.TBATS()`](https://ahaeusser.github.io/tscv/reference/forecast.TBATS.md),
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

model_frame
#> # A mable: 1 x 1
#>     TBATS
#>   <model>
#> 1 <TBATS>
# }
```
