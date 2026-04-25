# TBATS model

Train a TBATS model (Trigonometric seasonality, Box-Cox transformation,
ARMA errors, Trend and Seasonal components).

## Usage

``` r
train_tbats(.data, specials, periods, ...)
```

## Arguments

- .data:

  Input data as tsibble.

- specials:

  Specials as list defined in `specials_tbats`.

- periods:

  Integer vector. The periodicity of the time series (e.g.
  `periods = c(24, 168)` for hourly data).

- ...:

  Further arguments passed to
  [`forecast::tbats()`](https://pkg.robjhyndman.com/forecast/reference/tbats.html).

## Value

An object of class `TBATS`.
