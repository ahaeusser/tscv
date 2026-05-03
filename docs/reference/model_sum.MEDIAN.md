# Summarize a median model

Return a short model label for a fitted `MEDIAN` model.

## Usage

``` r
# S3 method for class 'MEDIAN'
model_sum(x)
```

## Arguments

- x:

  A fitted `MEDIAN` model object.

## Value

A character string.

## See also

Other MEDIAN:
[`MEDIAN()`](https://ahaeusser.github.io/tscv/reference/MEDIAN.md),
[`fitted.MEDIAN()`](https://ahaeusser.github.io/tscv/reference/fitted.MEDIAN.md),
[`forecast.MEDIAN()`](https://ahaeusser.github.io/tscv/reference/forecast.MEDIAN.md),
[`residuals.MEDIAN()`](https://ahaeusser.github.io/tscv/reference/residuals.MEDIAN.md)

## Examples

``` r
library(dplyr)
library(tsibble)
library(fabletools)

train_frame <- M4_monthly_data |>
  filter(series == first(series)) |>
  as_tsibble(index = index)

model_frame <- train_frame |>
  model("MEDIAN" = MEDIAN(value ~ window()))

model_frame
#> # A mable: 1 x 1
#>     MEDIAN
#>    <model>
#> 1 <MEDIAN>
```
