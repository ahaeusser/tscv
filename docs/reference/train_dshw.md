# Double Seasonal Holt-Winters model

Train a Double Seasonal Holt-Winters model (DSHW).

## Usage

``` r
train_dshw(.data, specials, periods, ...)
```

## Arguments

- .data:

  Input data as tsibble.

- specials:

  Specials as list defined in `specials_dshw`.

- periods:

  Integer vector. The periodicity of the time series (e.g.
  `periods = c(24, 168)` for hourly data).

- ...:

  Further arguments passed to
  [`forecast::dshw()`](https://pkg.robjhyndman.com/forecast/reference/dshw.html).

## Value

An object of class `DSHW`.
