# Extract fitted values from a median model

Extract fitted values from a fitted `MEDIAN` model.

## Usage

``` r
# S3 method for class 'MEDIAN'
fitted(object, ...)
```

## Arguments

- object:

  A fitted `MEDIAN` model object.

- ...:

  Additional arguments. Currently not used.

## Value

Fitted values.

## See also

Other MEDIAN:
[`MEDIAN()`](https://ahaeusser.github.io/tscv/reference/MEDIAN.md),
[`forecast.MEDIAN()`](https://ahaeusser.github.io/tscv/reference/forecast.MEDIAN.md),
[`model_sum.MEDIAN()`](https://ahaeusser.github.io/tscv/reference/model_sum.MEDIAN.md),
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

fitted(model_frame)
#> # A tsibble: 450 x 3 [1M]
#> # Key:       .model [1]
#>    .model    index .fitted
#>    <chr>     <mth>   <dbl>
#>  1 MEDIAN 1978 Feb   3278.
#>  2 MEDIAN 1978 Mrz   3278.
#>  3 MEDIAN 1978 Apr   3278.
#>  4 MEDIAN 1978 Mai   3278.
#>  5 MEDIAN 1978 Jun   3278.
#>  6 MEDIAN 1978 Jul   3278.
#>  7 MEDIAN 1978 Aug   3278.
#>  8 MEDIAN 1978 Sep   3278.
#>  9 MEDIAN 1978 Okt   3278.
#> 10 MEDIAN 1978 Nov   3278.
#> # ℹ 440 more rows
```
