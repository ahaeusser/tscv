# Extract fitted values from a Naive2 model

Extract fitted values from a fitted `NAIVE2` model.

## Usage

``` r
# S3 method for class 'NAIVE2'
fitted(object, ...)
```

## Arguments

- object:

  A fitted `NAIVE2` model object.

- ...:

  Additional arguments. Currently not used.

## Value

Fitted values.

## See also

Other NAIVE2:
[`NAIVE2()`](https://ahaeusser.github.io/tscv/reference/NAIVE2.md),
[`forecast.NAIVE2()`](https://ahaeusser.github.io/tscv/reference/forecast.NAIVE2.md),
[`model_sum.NAIVE2()`](https://ahaeusser.github.io/tscv/reference/model_sum.NAIVE2.md),
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

fitted(model_frame)
#> # A tsibble: 450 x 3 [1M]
#> # Key:       .model [1]
#>    .model    index .fitted
#>    <chr>     <mth>   <dbl>
#>  1 NAIVE2 1978 Feb     NA 
#>  2 NAIVE2 1978 Mrz   1517.
#>  3 NAIVE2 1978 Apr   1503.
#>  4 NAIVE2 1978 Mai   1529.
#>  5 NAIVE2 1978 Jun   1506.
#>  6 NAIVE2 1978 Jul   1485.
#>  7 NAIVE2 1978 Aug   1489.
#>  8 NAIVE2 1978 Sep   1569.
#>  9 NAIVE2 1978 Okt   1563.
#> 10 NAIVE2 1978 Nov   1585.
#> # ℹ 440 more rows
```
