# Extract residuals from a seasonal mean model

Extract residuals from a fitted `SMEAN` model.

## Usage

``` r
# S3 method for class 'SMEAN'
residuals(object, ...)
```

## Arguments

- object:

  A fitted `SMEAN` model object.

- ...:

  Additional arguments. Currently not used.

## Value

Residuals.

## See also

Other SMEAN:
[`SMEAN()`](https://ahaeusser.github.io/tscv/reference/SMEAN.md),
[`fitted.SMEAN()`](https://ahaeusser.github.io/tscv/reference/fitted.SMEAN.md),
[`forecast.SMEAN()`](https://ahaeusser.github.io/tscv/reference/forecast.SMEAN.md),
[`model_sum.SMEAN()`](https://ahaeusser.github.io/tscv/reference/model_sum.SMEAN.md)

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

residuals(model_frame)
#> # A tsibble: 450 x 3 [1M]
#> # Key:       .model [1]
#>    .model    index .resid
#>    <chr>     <mth>  <dbl>
#>  1 SMEAN  1978 Feb -1795.
#>  2 SMEAN  1978 Mrz -1825.
#>  3 SMEAN  1978 Apr -1815.
#>  4 SMEAN  1978 Mai -1856.
#>  5 SMEAN  1978 Jun -1883.
#>  6 SMEAN  1978 Jul -1882.
#>  7 SMEAN  1978 Aug -1766.
#>  8 SMEAN  1978 Sep -1795.
#>  9 SMEAN  1978 Okt -1764.
#> 10 SMEAN  1978 Nov -1831.
#> # ℹ 440 more rows
```
