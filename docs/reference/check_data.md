# Check, convert and shape the input data

The function checks whether the input data are a valid tsibble or not
(regular spaced in time and ordered). Furthermore, implicit missing
values are turned into explicit missing values (existing missing values
are left untouched). If the data are provided in wide format, they are
pivoted into long format.

## Usage

``` r
check_data(data, fill_missing = TRUE)
```

## Arguments

- data:

  A valid tsibble, either in long or in wide format.

- fill_missing:

  Logical value. If `TRUE`, implicit missing values are turned into
  explicit missing values.

## Value

data A valid tsibble in long format with one measurement variable.
