# Forecast a trained ELM model

Forecast a trained ELM model.

## Usage

``` r
# S3 method for class 'ELM'
forecast(object, new_data, specials = NULL, ...)
```

## Arguments

- object:

  An object of class `ELM`.

- new_data:

  Forecast horizon (n-step ahead forecast)

- specials:

  Specials are currently not in use.

- ...:

  Additional arguments for forecast method.

## Value

An object of class `fable`.
