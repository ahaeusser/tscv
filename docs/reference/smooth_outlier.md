# Identify and replace outliers

The function `smooth_outlier()` is a wrapper for
[`forecast::tsoutliers()`](https://pkg.robjhyndman.com/forecast/reference/tsoutliers.html),
working with numeric vectors. For non-seasonal time series, the supsmu
method is used. For seasonal time series, the series is decomposed via
STL and the IQR method is used on the remainder component. Values
outside the range are linear interpolated on the remainder and the
series is reconstructed with the corrected remainder component.

## Usage

``` r
smooth_outlier(x, periods, ...)
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
  [`forecast::tsoutliers()`](https://pkg.robjhyndman.com/forecast/reference/tsoutliers.html).

## Value

Numeric vector.
