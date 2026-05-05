# Convert data to a tsibble

Convert a `tibble` containing time series data to a `tsibble`.

## Usage

``` r
make_tsibble(main_frame, context)
```

## Arguments

- main_frame:

  A `tibble` containing the time series data.

- context:

  A named `list` with the identifiers for `series_id`, `value_id`, and
  `index_id`.

## Value

A `tsibble` with the same columns as `main_frame`, using the index and
key defined in `context`.

## Details

`make_tsibble()` is a small helper for time series cross-validation
workflows. It uses the time index and series identifier supplied in
`context` to create a regular `tsibble`.

The input data must contain the columns specified by `context$index_id`
and `context$series_id`. The column specified by `context$index_id` is
used as the time index, and the column specified by `context$series_id`
is used as the key.

## See also

Other time series cross-validation:
[`make_future()`](https://ahaeusser.github.io/tscv/reference/make_future.md),
[`make_split()`](https://ahaeusser.github.io/tscv/reference/make_split.md),
[`slice_test()`](https://ahaeusser.github.io/tscv/reference/slice_test.md),
[`slice_train()`](https://ahaeusser.github.io/tscv/reference/slice_train.md),
[`split_index()`](https://ahaeusser.github.io/tscv/reference/split_index.md)

## Examples

``` r
library(dplyr)
library(tsibble)

context <- list(
  series_id = "series",
  value_id = "value",
  index_id = "index"
)

main_frame <- M4_monthly_data |>
  filter(series %in% c("M23100", "M14395"))

tsibble_frame <- make_tsibble(
  main_frame = main_frame,
  context = context
)

tsibble_frame
#> # A tsibble: 354 x 4 [1M]
#> # Key:       series [2]
#>       index series category value
#>       <mth> <chr>  <chr>    <dbl>
#>  1 2001 Jul M14395 Micro    1116.
#>  2 2001 Aug M14395 Micro    1079.
#>  3 2001 Sep M14395 Micro     917.
#>  4 2001 Okt M14395 Micro     982.
#>  5 2001 Nov M14395 Micro     946.
#>  6 2001 Dez M14395 Micro     586.
#>  7 2002 Jan M14395 Micro     710.
#>  8 2002 Feb M14395 Micro     714.
#>  9 2002 Mrz M14395 Micro     675 
#> 10 2002 Apr M14395 Micro    1068.
#> # ℹ 344 more rows
```
