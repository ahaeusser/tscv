# Summarize a seasonal mean model

Return a short model label for a fitted `SMEAN` model.

## Usage

``` r
# S3 method for class 'SMEAN'
model_sum(x)
```

## Arguments

- x:

  A fitted `SMEAN` model object.

## Value

A character string.

## See also

Other SMEAN:
[`SMEAN()`](https://ahaeusser.github.io/tscv/reference/SMEAN.md),
[`fitted.SMEAN()`](https://ahaeusser.github.io/tscv/reference/fitted.SMEAN.md),
[`forecast.SMEAN()`](https://ahaeusser.github.io/tscv/reference/forecast.SMEAN.md),
[`residuals.SMEAN()`](https://ahaeusser.github.io/tscv/reference/residuals.SMEAN.md)

## Examples

``` r
library(dplyr)
library(tsibble)
library(fabletools)

train_frame <- M4_monthly_data |>
  filter(series == first(series)) |>
  as_tsibble(index = index)

model_frame <- train_frame |>
  model("SMEAN" = SMEAN(value ~ lag("year")))

model_frame
#> # A mable: 1 x 1
#>     SMEAN
#>   <model>
#> 1 <SMEAN>
```
