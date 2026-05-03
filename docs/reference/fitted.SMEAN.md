# Extract fitted values from a seasonal mean model

Extract fitted values from a fitted `SMEAN` model.

## Usage

``` r
# S3 method for class 'SMEAN'
fitted(object, ...)
```

## Arguments

- object:

  A fitted `SMEAN` model object.

- ...:

  Additional arguments. Currently not used.

## Value

Fitted values.

## See also

Other SMEAN:
[`SMEAN()`](https://ahaeusser.github.io/tscv/reference/SMEAN.md),
[`forecast.SMEAN()`](https://ahaeusser.github.io/tscv/reference/forecast.SMEAN.md),
[`model_sum.SMEAN()`](https://ahaeusser.github.io/tscv/reference/model_sum.SMEAN.md),
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

fitted(model_frame)
#> # A tsibble: 450 x 3 [1M]
#> # Key:       .model [1]
#>    .model    index .fitted
#>    <chr>     <mth>   <dbl>
#>  1 SMEAN  1978 Feb   3292.
#>  2 SMEAN  1978 Mrz   3334.
#>  3 SMEAN  1978 Apr   3336.
#>  4 SMEAN  1978 Mai   3363.
#>  5 SMEAN  1978 Jun   3369.
#>  6 SMEAN  1978 Jul   3377.
#>  7 SMEAN  1978 Aug   3309.
#>  8 SMEAN  1978 Sep   3373.
#>  9 SMEAN  1978 Okt   3348.
#> 10 SMEAN  1978 Nov   3358.
#> # ℹ 440 more rows
```
