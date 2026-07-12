# Forecast a Naive2 model

Forecast a fitted `NAIVE2` model.

## Usage

``` r
# S3 method for class 'NAIVE2'
forecast(object, new_data, specials = NULL, ...)
```

## Arguments

- object:

  A fitted `NAIVE2` model object.

- new_data:

  A `tsibble` containing future time points.

- specials:

  Parsed specials. Currently not used.

- ...:

  Additional arguments. Currently not used.

## Value

A vector of forecast distributions.

## Details

Point forecasts are calculated using `naive2()`. Forecast distributions
use a normal approximation based on a random walk applied to the
seasonally adjusted response. These forecast distributions are not part
of the original M4 Naive2 specification.

## See also

Other NAIVE2:
[`NAIVE2()`](https://ahaeusser.github.io/tscv/reference/NAIVE2.md),
[`fitted.NAIVE2()`](https://ahaeusser.github.io/tscv/reference/fitted.NAIVE2.md),
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

forecast(model_frame, h = 18)
#> # A fable: 18 x 4 [1M]
#> # Key:     .model [1]
#>    .model    index
#>    <chr>     <mth>
#>  1 NAIVE2 2015 Aug
#>  2 NAIVE2 2015 Sep
#>  3 NAIVE2 2015 Okt
#>  4 NAIVE2 2015 Nov
#>  5 NAIVE2 2015 Dez
#>  6 NAIVE2 2016 Jan
#>  7 NAIVE2 2016 Feb
#>  8 NAIVE2 2016 Mrz
#>  9 NAIVE2 2016 Apr
#> 10 NAIVE2 2016 Mai
#> 11 NAIVE2 2016 Jun
#> 12 NAIVE2 2016 Jul
#> 13 NAIVE2 2016 Aug
#> 14 NAIVE2 2016 Sep
#> 15 NAIVE2 2016 Okt
#> 16 NAIVE2 2016 Nov
#> 17 NAIVE2 2016 Dez
#> 18 NAIVE2 2017 Jan
#> # ℹ 2 more variables: value <dist>, .mean <dbl>
```
