# Automatic training of MLPs

Automatic train a Multilayer Perceptron (MLPs) model. This function is a
wrapper for [`nnfor::mlp()`](https://rdrr.io/pkg/nnfor/man/mlp.html).

## Usage

``` r
MLP(formula, ...)
```

## Arguments

- formula:

  Model specification (see "Specials" section, currently not in use...)

- ...:

  Further arguments passed to
  [`nnfor::mlp()`](https://rdrr.io/pkg/nnfor/man/mlp.html).

## Value

mlp_model An object of class `MLP`.
