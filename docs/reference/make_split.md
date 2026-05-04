# Create train-test splits for time series cross-validation

Create a split frame with train and test indices for one or more time
series.

## Usage

``` r
make_split(
  main_frame,
  context,
  type,
  value,
  n_ahead,
  n_skip = 0,
  n_lag = 0,
  mode = "slide",
  exceed = TRUE
)
```

## Arguments

- main_frame:

  A `tibble` containing the time series data.

- context:

  A named `list` with the identifiers for `series_id`, `value_id`, and
  `index_id`.

- type:

  Character value. The type of initial split. Possible values are
  `"first"`, `"last"`, and `"prob"`.

- value:

  Numeric value specifying the initial split.

- n_ahead:

  Integer. The forecast horizon, i.e. the number of observations in each
  test window.

- n_skip:

  Integer. The number of observations to skip between split origins. The
  default is `0`.

- n_lag:

  Integer. The number of lagged observations to include before the test
  window. This is useful if lagged predictors are required when
  constructing test features. The default is `0`.

- mode:

  Character value. Either `"slide"` for a fixed-window approach or
  `"stretch"` for an expanding-window approach.

- exceed:

  Logical value. If `TRUE`, out-of-sample splits exceeding the original
  sample size are created.

## Value

A `tibble` containing the split plan. The output has one row per time
series and split, with list-columns `train` and `test` containing
integer row positions.

## Details

`make_split()` creates rolling-origin train-test splits for time series
cross-validation. The output is used by functions such as
[`slice_train()`](https://ahaeusser.github.io/tscv/reference/slice_train.md)
and
[`slice_test()`](https://ahaeusser.github.io/tscv/reference/slice_test.md)
to extract the corresponding training and testing samples from
`main_frame`.

The function supports two training-window modes:

- `mode = "slide"` creates a fixed-window approach. The training window
  has constant length and moves forward over time.

- `mode = "stretch"` creates an expanding-window approach. The training
  window starts at the first observation and grows over time.

The initial training window is controlled by `type` and `value`:

- `type = "first"` uses the first `value` observations as the initial
  training window.

- `type = "last"` keeps the last `value` observations for testing and
  derives the initial training window from the remaining sample.

- `type = "prob"` uses `floor(value * n_total)` observations as the
  initial training window.

The argument `n_skip` controls how far the rolling origin moves between
consecutive splits. For non-overlapping test windows, use
`n_skip = n_ahead - 1`.

## See also

Other time series cross-validation:
[`make_future()`](https://ahaeusser.github.io/tscv/reference/make_future.md),
[`slice_test()`](https://ahaeusser.github.io/tscv/reference/slice_test.md),
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

# Fixed-window split plan
fixed_split <- make_split(
  main_frame = main_frame,
  context = context,
  type = "first",
  value = 2400,
  n_ahead = 24,
  n_skip = 23,
  n_lag = 0,
  mode = "slide",
  exceed = FALSE
)

fixed_split
#> # A tibble: 1,262 × 4
#>    bidding_zone split train         test      
#>    <chr>        <int> <list>        <list>    
#>  1 DE               1 <int [2,400]> <int [24]>
#>  2 DE               2 <int [2,400]> <int [24]>
#>  3 DE               3 <int [2,400]> <int [24]>
#>  4 DE               4 <int [2,400]> <int [24]>
#>  5 DE               5 <int [2,400]> <int [24]>
#>  6 DE               6 <int [2,400]> <int [24]>
#>  7 DE               7 <int [2,400]> <int [24]>
#>  8 DE               8 <int [2,400]> <int [24]>
#>  9 DE               9 <int [2,400]> <int [24]>
#> 10 DE              10 <int [2,400]> <int [24]>
#> # ℹ 1,252 more rows

# Expanding-window split plan
expanding_split <- make_split(
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

expanding_split
#> # A tibble: 1,262 × 4
#>    bidding_zone split train         test      
#>    <chr>        <int> <list>        <list>    
#>  1 DE               1 <int [2,400]> <int [24]>
#>  2 DE               2 <int [2,424]> <int [24]>
#>  3 DE               3 <int [2,448]> <int [24]>
#>  4 DE               4 <int [2,472]> <int [24]>
#>  5 DE               5 <int [2,496]> <int [24]>
#>  6 DE               6 <int [2,520]> <int [24]>
#>  7 DE               7 <int [2,544]> <int [24]>
#>  8 DE               8 <int [2,568]> <int [24]>
#>  9 DE               9 <int [2,592]> <int [24]>
#> 10 DE              10 <int [2,616]> <int [24]>
#> # ℹ 1,252 more rows
```
