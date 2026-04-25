# Forecast a trained DSHW model

Forecast a trained DSHW model.

## Usage

``` r
# S3 method for class 'DSHW'
forecast(object, new_data, specials = NULL, ...)
```

## Arguments

- object:

  An object of class `DSHW`.

- new_data:

  Forecast horizon (n-step ahead forecast)

- specials:

  Specials are currently not in use.

- ...:

  Additional arguments for forecast method.

## Value

An object of class `fable`.
