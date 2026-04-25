# Multilayer Perceptron (MLP)

Train a Multilayer Perceptron (MLP) model.

## Usage

``` r
train_mlp(.data, specials, n_seed = 42, ...)
```

## Arguments

- .data:

  Input data as tsibble.

- specials:

  Specials as list defined in `specials_mlp`.

- n_seed:

  Integer value. The seed for the random number generator (for
  reproducibility).

- ...:

  Further arguments passed to
  [`nnfor::mlp()`](https://rdrr.io/pkg/nnfor/man/mlp.html).

## Value

An object of class `MLP`.
