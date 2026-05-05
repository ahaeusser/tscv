# Slice training data from a split frame

Extract training observations from a complete time series data set
according to a split plan created by
[`make_split()`](https://ahaeusser.github.io/tscv/reference/make_split.md).

## Usage

``` r
slice_train(main_frame, split_frame, context)
```

## Arguments

- main_frame:

  A `tibble` containing the complete time series data.

- split_frame:

  A `tibble` containing train and test indices, usually created by
  [`make_split()`](https://ahaeusser.github.io/tscv/reference/make_split.md).

- context:

  A named `list` with the identifiers for `series_id`, `value_id`, and
  `index_id`.

## Value

A `tibble` containing the sliced training data. It contains the same
columns as `main_frame`, plus a `split` column.

## Details

`slice_train()` uses the row positions stored in the `train` list-column
of `split_frame` to extract the corresponding observations from
`main_frame`. The function is designed for rolling-origin time series
cross-validation workflows.

The returned data has the same columns as `main_frame`, plus a `split`
column identifying the train-test split. If `main_frame` contains
multiple time series, slicing is performed separately for each series
using the series identifier supplied in `context`.

## See also

Other time series cross-validation:
[`make_future()`](https://ahaeusser.github.io/tscv/reference/make_future.md),
[`make_split()`](https://ahaeusser.github.io/tscv/reference/make_split.md),
[`make_tsibble()`](https://ahaeusser.github.io/tscv/reference/make_tsibble.md),
[`slice_test()`](https://ahaeusser.github.io/tscv/reference/slice_test.md),
[`split_index()`](https://ahaeusser.github.io/tscv/reference/split_index.md)

## Examples

``` r
library(dplyr)

context <- list(
  series_id = "series",
  value_id = "value",
  index_id = "index"
)

main_frame <- M4_monthly_data |>
  filter(series == "M23100")

split_frame <- make_split(
  main_frame = main_frame,
  context = context,
  type = "first",
  value = 120,
  n_ahead = 18,
  n_skip = 17,
  n_lag = 0,
  mode = "stretch",
  exceed = FALSE
)

train_frame <- slice_train(
  main_frame = main_frame,
  split_frame = split_frame,
  context = context
)

train_frame
#> # A tibble: 258 × 5
#>       index series category    value split
#>       <mth> <chr>  <chr>       <dbl> <int>
#>  1 2003 Jan M23100 Demographic  8240     1
#>  2 2003 Feb M23100 Demographic  8160     1
#>  3 2003 Mrz M23100 Demographic  8300     1
#>  4 2003 Apr M23100 Demographic  8580     1
#>  5 2003 Mai M23100 Demographic  8860     1
#>  6 2003 Jun M23100 Demographic  8980     1
#>  7 2003 Jul M23100 Demographic  8860     1
#>  8 2003 Aug M23100 Demographic  8970     1
#>  9 2003 Sep M23100 Demographic  8840     1
#> 10 2003 Okt M23100 Demographic  8740     1
#> # ℹ 248 more rows
```
