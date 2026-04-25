# Assign objects within a list to an environment

`lst_to_env` is a helper function that assigns the objects within a list
to an environment.

## Usage

``` r
lst_to_env(x, envir = .GlobalEnv, ...)
```

## Arguments

- x:

  A list containing the objects to assign.

- envir:

  The environment to use (default is `.GlobalEnv`).

- ...:

  Further arguments passed to
  [`assign()`](https://rdrr.io/r/base/assign.html).
