# Extreme Learning Machine (ELM)

Automatic train an Extreme Learning Machines (ELMs) model. This function
is a wrapper for
[`nnfor::elm()`](https://rdrr.io/pkg/nnfor/man/elm.html).

## Usage

``` r
ELM(formula, ...)
```

## Arguments

- formula:

  Model specification (see "Specials" section, currently not in use...)

- ...:

  Further arguments passed to
  [`nnfor::elm()`](https://rdrr.io/pkg/nnfor/man/elm.html).

## Value

elm_model An object of class `ELM`.
