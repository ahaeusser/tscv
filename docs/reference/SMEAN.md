# Seasonal mean model

Automatic train a seasonal mean model (SMEAN). This is equivalent to a
linear regression against seasonal dummy variables only, i.e.
`TSLM(value ~ season())`.

## Usage

``` r
SMEAN(formula, ...)
```

## Arguments

- formula:

  Model specification (see "Specials" section, currently not in use
  ...).

- ...:

  Further arguments.

## Value

smean_model An object of class `SMEAN`.
