# Extreme Learning Machine (ELM)

Train an Extreme Learning Machine (ELM) model.

## Usage

``` r
train_elm(.data, specials, n_seed = 42, ...)
```

## Arguments

- .data:

  Input data as tsibble.

- specials:

  Specials as list defined in `specials_elm`.

- n_seed:

  Integer value. The seed for the random number generator (for
  reproducibility).

- ...:

  Further arguments passed to
  [`nnfor::elm()`](https://rdrr.io/pkg/nnfor/man/elm.html).

## Value

An object of class `ELM`.
