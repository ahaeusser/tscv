# Estimate the sample partial autocorrelation of a numeric vector

`pacf_vec` estimates the sample partial autocorrelation function of a
numeric vector.

## Usage

``` r
pacf_vec(x, lag_max = 24, ...)
```

## Arguments

- x:

  Numeric vector.

- lag_max:

  Maximum lag as integer.

- ...:

  Further arguments passed to
  [`stats::pacf()`](https://rdrr.io/r/stats/acf.html).

## Value

x Numeric vector.
