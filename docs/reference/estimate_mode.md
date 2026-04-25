# Estimate mode of a distribution based on Kernel Density Estimation

The function estimates the mode of a distribution based on Kernel
Density Estimation.

## Usage

``` r
estimate_mode(x, na_rm = TRUE, ...)
```

## Arguments

- x:

  Numeric vector.

- na_rm:

  Logical value. If `TRUE`, missing values are dropped.

- ...:

  Further arguments passed to `stats::densitiy()`.

## Value

mode Numeric value. The mode of the distribution.
