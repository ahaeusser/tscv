# Forecast a trained seasonal median model

Forecast a trained seasonal median model.

## Usage

``` r
# S3 method for class 'SMEDIAN'
forecast(object, new_data, specials = NULL, ...)
```

## Arguments

- object:

  An object of class `SMEDIAN`.

- new_data:

  Forecast horizon (n-step ahead forecast)

- specials:

  Specials are currently not in use.

- ...:

  Additional arguments for forecast method.

## Value

An object of class `fable`.
