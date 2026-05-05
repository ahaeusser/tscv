# Summarise train-test splits

Summarise the time and row-index ranges of training and test samples.

## Usage

``` r
summarise_split(data)
```

## Arguments

- data:

  A valid `tsibble` in long format. It must contain the columns `split`,
  `sample`, and `id`.

## Value

A `tibble` containing the summarized split ranges.

## Details

`summarise_split()` is intended for data sets that contain sliced
training and test observations from a time series cross-validation
workflow. The input must be a `tsibble` with the columns `split`,
`sample`, and `id`:

- `split`: train-test split identifier;

- `sample`: sample label, usually `"train"` or `"test"`;

- `id`: integer row index within the original time series.

The function returns one row per split. For each split, it reports the
time range and index range of each sample.

## See also

Other data analysis:
[`acf_vec()`](https://ahaeusser.github.io/tscv/reference/acf_vec.md),
[`estimate_acf()`](https://ahaeusser.github.io/tscv/reference/estimate_acf.md),
[`estimate_kurtosis()`](https://ahaeusser.github.io/tscv/reference/estimate_kurtosis.md),
[`estimate_mode()`](https://ahaeusser.github.io/tscv/reference/estimate_mode.md),
[`estimate_pacf()`](https://ahaeusser.github.io/tscv/reference/estimate_pacf.md),
[`estimate_skewness()`](https://ahaeusser.github.io/tscv/reference/estimate_skewness.md),
[`pacf_vec()`](https://ahaeusser.github.io/tscv/reference/pacf_vec.md),
[`summarise_data()`](https://ahaeusser.github.io/tscv/reference/summarise_data.md),
[`summarise_stats()`](https://ahaeusser.github.io/tscv/reference/summarise_stats.md)

## Examples

``` r
library(dplyr)
library(tsibble)

context <- list(
  series_id = "bidding_zone",
  value_id = "value",
  index_id = "time"
)

main_frame <- elec_price |>
  filter(bidding_zone == "DE") |>
  slice_head(n = 120)

split_frame <- make_split(
  main_frame = main_frame,
  context = context,
  type = "first",
  value = 48,
  n_ahead = 24,
  n_skip = 23,
  n_lag = 0,
  mode = "stretch",
  exceed = FALSE
)

train_frame <- slice_train(
  main_frame = main_frame,
  split_frame = split_frame,
  context = context
) |>
  mutate(sample = "train")

test_frame <- slice_test(
  main_frame = main_frame,
  split_frame = split_frame,
  context = context
) |>
  mutate(sample = "test")

split_data <- bind_rows(train_frame, test_frame) |>
  group_by(bidding_zone, split, sample) |>
  mutate(id = row_number()) |>
  ungroup() |>
  as_tsibble(
    index = time,
    key = c(bidding_zone, split, sample)
  )

summarise_split(split_data)
#> # A tibble: 3 × 5
#>   split time_test                         time_train      index_test index_train
#>   <int> <chr>                             <chr>           <chr>      <chr>      
#> 1     1 [2019-01-03, 2019-01-03 23:00:00] [2019-01-01, 2… [01, 24]   [01, 48]   
#> 2     2 [2019-01-04, 2019-01-04 23:00:00] [2019-01-01, 2… [01, 24]   [01, 72]   
#> 3     3 [2019-01-05, 2019-01-05 23:00:00] [2019-01-01, 2… [01, 24]   [01, 96]   
```
