# Extract residuals from a median model

Extract residuals from a fitted `MEDIAN` model.

## Usage

``` r
# S3 method for class 'MEDIAN'
residuals(object, ...)
```

## Arguments

- object:

  A fitted `MEDIAN` model object.

- ...:

  Additional arguments. Currently not used.

## Value

Residuals.

## See also

Other MEDIAN:
[`MEDIAN()`](https://ahaeusser.github.io/tscv/reference/MEDIAN.md),
[`fitted.MEDIAN()`](https://ahaeusser.github.io/tscv/reference/fitted.MEDIAN.md),
[`forecast.MEDIAN()`](https://ahaeusser.github.io/tscv/reference/forecast.MEDIAN.md),
[`model_sum.MEDIAN()`](https://ahaeusser.github.io/tscv/reference/model_sum.MEDIAN.md)

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

residuals(model_frame)
#> # A tsibble: 450 x 3 [1M]
#> # Key:       .model [1]
#>    .model    index .resid
#>    <chr>     <mth>  <dbl>
#>  1 MEDIAN 1978 Feb -1780.
#>  2 MEDIAN 1978 Mrz -1770.
#>  3 MEDIAN 1978 Apr -1757.
#>  4 MEDIAN 1978 Mai -1771.
#>  5 MEDIAN 1978 Jun -1792.
#>  6 MEDIAN 1978 Jul -1784.
#>  7 MEDIAN 1978 Aug -1735.
#>  8 MEDIAN 1978 Sep -1700.
#>  9 MEDIAN 1978 Okt -1694.
#> 10 MEDIAN 1978 Nov -1751.
#> # ℹ 440 more rows
```
