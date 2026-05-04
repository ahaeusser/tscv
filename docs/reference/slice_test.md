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
[`slice_train()`](https://ahaeusser.github.io/tscv/reference/slice_train.md),
[`split_index()`](https://ahaeusser.github.io/tscv/reference/split_index.md)

## Examples

``` r
library(dplyr)

context <- list(
  series_id = "bidding_zone",
  value_id = "value",
  index_id = "time"
)

main_frame <- elec_price |>
  filter(bidding_zone %in% c("DE", "FR"))

split_frame <- make_split(
  main_frame = main_frame,
  context = context,
  type = "first",
  value = 2400,
  n_ahead = 24,
  n_skip = 23,
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
#> # A tibble: 30,288 × 6
#>    time                item            unit      bidding_zone value split
#>    <dttm>              <chr>           <chr>     <chr>        <dbl> <int>
#>  1 2019-04-11 00:00:00 Day-ahead Price [EUR/MWh] DE            37.1     1
#>  2 2019-04-11 01:00:00 Day-ahead Price [EUR/MWh] DE            36.5     1
#>  3 2019-04-11 02:00:00 Day-ahead Price [EUR/MWh] DE            37.1     1
#>  4 2019-04-11 03:00:00 Day-ahead Price [EUR/MWh] DE            38.9     1
#>  5 2019-04-11 04:00:00 Day-ahead Price [EUR/MWh] DE            47.9     1
#>  6 2019-04-11 05:00:00 Day-ahead Price [EUR/MWh] DE            56.4     1
#>  7 2019-04-11 06:00:00 Day-ahead Price [EUR/MWh] DE            59.2     1
#>  8 2019-04-11 07:00:00 Day-ahead Price [EUR/MWh] DE            51.7     1
#>  9 2019-04-11 08:00:00 Day-ahead Price [EUR/MWh] DE            48.6     1
#> 10 2019-04-11 09:00:00 Day-ahead Price [EUR/MWh] DE            46.0     1
#> # ℹ 30,278 more rows
```
