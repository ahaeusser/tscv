# Forecast a median model

Forecast a fitted `MEDIAN` model.

## Usage

``` r
# S3 method for class 'MEDIAN'
forecast(object, new_data, specials = NULL, ...)
```

## Arguments

- object:

  A fitted `MEDIAN` model object.

- new_data:

  A `tsibble` containing future time points.

- specials:

  Parsed specials. Currently not used.

- ...:

  Additional arguments. Currently not used.

## Value

A vector of forecast distributions.

## See also

Other MEDIAN:
[`MEDIAN()`](https://ahaeusser.github.io/tscv/reference/MEDIAN.md),
[`fitted.MEDIAN()`](https://ahaeusser.github.io/tscv/reference/fitted.MEDIAN.md),
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

forecast(model_frame, h = 12)
#> # A fable: 12 x 4 [1M]
#> # Key:     .model [1]
#>    .model    index
#>    <chr>     <mth>
#>  1 MEDIAN 2015 Aug
#>  2 MEDIAN 2015 Sep
#>  3 MEDIAN 2015 Okt
#>  4 MEDIAN 2015 Nov
#>  5 MEDIAN 2015 Dez
#>  6 MEDIAN 2016 Jan
#>  7 MEDIAN 2016 Feb
#>  8 MEDIAN 2016 Mrz
#>  9 MEDIAN 2016 Apr
#> 10 MEDIAN 2016 Mai
#> 11 MEDIAN 2016 Jun
#> 12 MEDIAN 2016 Jul
#> # ℹ 2 more variables: value <dist>, .mean <dbl>
```
