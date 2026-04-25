# Forecast a trained seasonal naive model

Forecast a trained seasonal naive model.

## Usage

``` r
# S3 method for class 'SNAIVE2'
forecast(object, new_data, specials = NULL, ...)
```

## Arguments

- object:

  An object of class `SNAIVE2`.

- new_data:

  Forecast horizon (n-step ahead forecast)

- specials:

  Specials are currently not in use.

- ...:

  Additional arguments for forecast method.

## Value

An object of class `fable`.
