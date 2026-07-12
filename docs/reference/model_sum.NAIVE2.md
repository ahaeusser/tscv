# Summarize a Naive2 model

Return a short model label for a fitted `NAIVE2` model.

## Usage

``` r
# S3 method for class 'NAIVE2'
model_sum(x)
```

## Arguments

- x:

  A fitted `NAIVE2` model object.

## Value

A character string.

## See also

Other NAIVE2:
[`NAIVE2()`](https://ahaeusser.github.io/tscv/reference/NAIVE2.md),
[`fitted.NAIVE2()`](https://ahaeusser.github.io/tscv/reference/fitted.NAIVE2.md),
[`forecast.NAIVE2()`](https://ahaeusser.github.io/tscv/reference/forecast.NAIVE2.md),
[`residuals.NAIVE2()`](https://ahaeusser.github.io/tscv/reference/residuals.NAIVE2.md)

## Examples

``` r
library(dplyr)
library(tsibble)
library(fabletools)

train_frame <- M4_monthly_data |>
  filter(series == first(series)) |>
  as_tsibble(index = index)

model_frame <- train_frame |>
  model("NAIVE2" = NAIVE2(value ~ season(12)))

model_frame
#> # A mable: 1 x 1
#>     NAIVE2
#>    <model>
#> 1 <NAIVE2>
```
