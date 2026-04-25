# Forecast a trained EXPERT model

Forecast a trained EXPERT model.

## Usage

``` r
# S3 method for class 'EXPERT'
forecast(object, new_data, specials = NULL, ...)
```

## Arguments

- object:

  An object of class `EXPERT`.

- new_data:

  Forecast horizon (n-step ahead forecast)

- specials:

  Specials are currently not in use.

- ...:

  Additional arguments for forecast method.

## Value

An object of class `fable`.
