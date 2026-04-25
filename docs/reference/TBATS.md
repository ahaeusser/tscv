# Automatic train a TBATS model

Automatic train a TBATS model (Trigonometric seasonality, Box-Cox
transformation, ARMA errors, Trend and Seasonal components). This
function is a wrapper for
[`forecast::tbats()`](https://pkg.robjhyndman.com/forecast/reference/tbats.html).

## Usage

``` r
TBATS(formula, ...)
```

## Arguments

- formula:

  Model specification (see "Specials" section, currently not in use ...)

- ...:

  Further arguments passed to
  [`forecast::tbats()`](https://pkg.robjhyndman.com/forecast/reference/tbats.html).

## Value

tbats_model An object of class `TBATS`.
