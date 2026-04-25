# Interpolate missing values

The function `interpolate_missing()` is a wrapper for
[`forecast::na.interp()`](https://pkg.robjhyndman.com/forecast/reference/na.interp.html),
working with numeric vectors. For non-seasonal time series, linear
interpolation is used and for seasonal time series, the series is
decomposed via STL and the seasonally adjusted series is linearly
interpolated and the seasonal component is added back.

## Usage

``` r
interpolate_missing(x, periods, ...)
```

## Arguments

- x:

  Numeric vector.

- periods:

  Numeric vector. The seasonal periods of the time series.

- ...:

  Further arguments passed to
  [`forecast::msts()`](https://pkg.robjhyndman.com/forecast/reference/msts.html)
  or
  [`forecast::na.interp()`](https://pkg.robjhyndman.com/forecast/reference/na.interp.html).

## Value

Numeric vector.
