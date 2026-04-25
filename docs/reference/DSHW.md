# Automatic train a DSHW model

Automatic train a Double Seasonal Holt-Winters model (DSHW). This
function is a wrapper for
[`forecast::dshw()`](https://pkg.robjhyndman.com/forecast/reference/dshw.html).

## Usage

``` r
DSHW(formula, ...)
```

## Arguments

- formula:

  Model specification (see "Specials" section, currently not in use ...)

- ...:

  Further arguments passed to
  [`forecast::dshw()`](https://pkg.robjhyndman.com/forecast/reference/dshw.html).

## Value

dshw_model An object of class `DSHW`.
