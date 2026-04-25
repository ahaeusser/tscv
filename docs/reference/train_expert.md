# EXPERT model

Train the EXPERT model.

## Usage

``` r
train_expert(.data, specials = NULL, periods, xreg = NULL, ...)
```

## Arguments

- .data:

  Input data as tsibble.

- specials:

  Specials as list defined in `specials_expert`.

- periods:

  Integer vector. The periodicity of the time series (e.g.
  `periods = c(24, 168)` for hourly data).

- xreg:

  A `tsibble` containing exogenous variables.

- ...:

  Further arguments (currently not in use).

## Value

An object of class `EXPERT`.
