# Seasonal mean model

Train a seasonal mean model (SMEAN). This is equivalent to a linear
regression against seasonal dummy variables only, i.e.
`TSLM(value ~ season())`.

## Usage

``` r
train_smean(.data, specials, ...)
```

## Arguments

- .data:

  Input data as tsibble.

- specials:

  Specials as list defined in `specials_smean`.

- ...:

  Currently not in use.

## Value

An object of class `SMEAN`.
