# Slice test data from a split frame

Extract test observations from a complete time series data set according
to a split plan created by
[`make_split()`](https://ahaeusser.github.io/tscv/reference/make_split.md).

## Usage

``` r
slice_test(main_frame, split_frame, context)
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

A `tibble` containing the sliced test data. It contains the same columns
as `main_frame`, plus a `split` column.

## Details

`slice_test()` uses the row positions stored in the `test` list-column
of `split_frame` to extract the corresponding observations from
`main_frame`. The function is designed for rolling-origin time series
cross-validation workflows.

The returned data has the same columns as `main_frame`, plus a `split`
column identifying the train-test split. If `main_frame` contains
multiple time series, slicing is performed separately for each series
using the series identifier supplied in `context`.

When
[`make_split()`](https://ahaeusser.github.io/tscv/reference/make_split.md)
was called with `n_lag > 0`, the test data may include lagged
observations before the forecast horizon.

## See also

Other time series cross-validation:
[`make_future()`](https://ahaeusser.github.io/tscv/reference/make_future.md),
[`make_split()`](https://ahaeusser.github.io/tscv/reference/make_split.md),
[`make_tsibble()`](https://ahaeusser.github.io/tscv/reference/make_tsibble.md),
[`slice_train()`](https://ahaeusser.github.io/tscv/reference/slice_train.md),
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

test_frame <- slice_test(
  main_frame = main_frame,
  split_frame = split_frame,
  context = context
)

test_frame
#> # A tibble: 36 × 5
#>       index series category    value split
#>       <mth> <chr>  <chr>       <dbl> <int>
#>  1 2013 Jan M23100 Demographic  8940     1
#>  2 2013 Feb M23100 Demographic  8950     1
#>  3 2013 Mrz M23100 Demographic  9080     1
#>  4 2013 Apr M23100 Demographic  9270     1
#>  5 2013 Mai M23100 Demographic  9570     1
#>  6 2013 Jun M23100 Demographic  9610     1
#>  7 2013 Jul M23100 Demographic  9510     1
#>  8 2013 Aug M23100 Demographic  9550     1
#>  9 2013 Sep M23100 Demographic  9400     1
#> 10 2013 Okt M23100 Demographic  9310     1
#> # ℹ 26 more rows
```
