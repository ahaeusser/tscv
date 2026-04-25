# Forecast a trained TBATS model

Forecast a trained TBATS model.

## Usage

``` r
# S3 method for class 'TBATS'
forecast(object, new_data, specials = NULL, ...)
```

## Arguments

- object:

  An object of class `TBATS`.

- new_data:

  Forecast horizon (n-step ahead forecast)

- specials:

  Specials are currently not in use.

- ...:

  Additional arguments for forecast method.

## Value

An object of class `fable`.
