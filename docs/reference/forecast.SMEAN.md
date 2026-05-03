# Forecast a seasonal mean model

Forecast a fitted `SMEAN` model.

## Usage

``` r
# S3 method for class 'SMEAN'
forecast(object, new_data, specials = NULL, ...)
```

## Arguments

- object:

  A fitted `SMEAN` model object.

- new_data:

  A `tsibble` containing future time points.

- specials:

  Parsed specials. Currently not used.

- ...:

  Additional arguments. Currently not used.

## Value

A vector of forecast distributions.

## See also

Other SMEAN:
[`SMEAN()`](https://ahaeusser.github.io/tscv/reference/SMEAN.md),
[`fitted.SMEAN()`](https://ahaeusser.github.io/tscv/reference/fitted.SMEAN.md),
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

forecast(model_frame, h = 12)
#> # A fable: 12 x 4 [1M]
#> # Key:     .model [1]
#>    .model    index
#>    <chr>     <mth>
#>  1 SMEAN  2015 Aug
#>  2 SMEAN  2015 Sep
#>  3 SMEAN  2015 Okt
#>  4 SMEAN  2015 Nov
#>  5 SMEAN  2015 Dez
#>  6 SMEAN  2016 Jan
#>  7 SMEAN  2016 Feb
#>  8 SMEAN  2016 Mrz
#>  9 SMEAN  2016 Apr
#> 10 SMEAN  2016 Mai
#> 11 SMEAN  2016 Jun
#> 12 SMEAN  2016 Jul
#> # ℹ 2 more variables: value <dist>, .mean <dbl>
```
