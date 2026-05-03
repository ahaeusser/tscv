# Summarize a seasonal median model

Return a short model label for a fitted `SMEDIAN` model.

## Usage

``` r
# S3 method for class 'SMEDIAN'
model_sum(x)
```

## Arguments

- x:

  A fitted `SMEDIAN` model object.

## Value

A character string.

## See also

Other SMEDIAN:
[`SMEDIAN()`](https://ahaeusser.github.io/tscv/reference/SMEDIAN.md),
[`fitted.SMEDIAN()`](https://ahaeusser.github.io/tscv/reference/fitted.SMEDIAN.md),
[`forecast.SMEDIAN()`](https://ahaeusser.github.io/tscv/reference/forecast.SMEDIAN.md),
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

model_frame
#> # A mable: 1 x 1
#>     SMEDIAN
#>     <model>
#> 1 <SMEDIAN>
```
