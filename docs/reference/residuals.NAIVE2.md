# Extract residuals from a Naive2 model

Extract residuals from a fitted `NAIVE2` model.

## Usage

``` r
# S3 method for class 'NAIVE2'
residuals(object, ...)
```

## Arguments

- object:

  A fitted `NAIVE2` model object.

- ...:

  Additional arguments. Currently not used.

## Value

Residuals.

## See also

Other NAIVE2:
[`NAIVE2()`](https://ahaeusser.github.io/tscv/reference/NAIVE2.md),
[`fitted.NAIVE2()`](https://ahaeusser.github.io/tscv/reference/fitted.NAIVE2.md),
[`forecast.NAIVE2()`](https://ahaeusser.github.io/tscv/reference/forecast.NAIVE2.md),
[`model_sum.NAIVE2()`](https://ahaeusser.github.io/tscv/reference/model_sum.NAIVE2.md)

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

residuals(model_frame)
#> # A tsibble: 450 x 3 [1M]
#> # Key:       .model [1]
#>    .model    index .resid
#>    <chr>     <mth>  <dbl>
#>  1 NAIVE2 1978 Feb  NA   
#>  2 NAIVE2 1978 Mrz  -8.55
#>  3 NAIVE2 1978 Apr  18.0 
#>  4 NAIVE2 1978 Mai -21.8 
#>  5 NAIVE2 1978 Jun -19.9 
#>  6 NAIVE2 1978 Jul   9.29
#>  7 NAIVE2 1978 Aug  54.5 
#>  8 NAIVE2 1978 Sep   9.86
#>  9 NAIVE2 1978 Okt  20.8 
#> 10 NAIVE2 1978 Nov -58.1 
#> # ℹ 440 more rows
```
