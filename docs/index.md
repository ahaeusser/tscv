# tscv

The package `tscv` provides helper functions for time series analysis,
forecasting and time series cross-validation. It is mainly designed to
work with the tidy forecasting ecosystem, especially the packages
`tsibble`, `fable`, `fabletools` and `feasts`.

The package contains tools for:

- creating **rolling-origin** resampling schemes for time series
  cross-validation
- slicing **training and test samples** from time-indexed data
- converting forecasts into a common format for evaluation
- calculating **forecast accuracy measures**
- **visualizing time series data**, forecast errors and distributional
  properties
- fitting **additional benchmark and forecasting models** compatible
  with `fable`
- working with example time series data sets

The main focus of the package is to simplify repeated forecasting
experiments across several time series, models, forecast horizons and
rolling-origin splits.

## Installation

You can install the development version from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("ahaeusser/tscv")
```

## Basic workflow

A typical workflow with `tscv` consists of the following steps:

1.  Prepare the data in long format.
2.  Define a `context` object identifying the series, value and index
    columns.
3.  Create rolling-origin splits using
    [`make_split()`](https://ahaeusser.github.io/tscv/reference/make_split.md).
4.  Slice the training and test data using
    [`slice_train()`](https://ahaeusser.github.io/tscv/reference/slice_train.md)
    and
    [`slice_test()`](https://ahaeusser.github.io/tscv/reference/slice_test.md).
5.  Estimate forecasting models using `fable` or the additional models
    provided by `tscv`.
6.  Convert forecasts with
    [`make_future()`](https://ahaeusser.github.io/tscv/reference/make_future.md).
7.  Evaluate forecast accuracy with
    [`make_accuracy()`](https://ahaeusser.github.io/tscv/reference/make_accuracy.md).
8.  Visualize the data, forecasts or accuracy measures.

For example, the `context` object defines the column names used by many
functions:

``` r
context <- list(
  series_id = "bidding_zone",
  value_id = "value",
  index_id = "time"
)
```

This makes the package flexible with respect to different data sets, as
long as the data is provided in a tidy long format.

## Function overview

The following table summarizes the main functions in `tscv` by topic.

| **Topic** | **Function(s)** | **Description** |
|:---|:---|:---|
| **Time series cross-validation** | [`make_split()`](https://ahaeusser.github.io/tscv/reference/make_split.md), [`slice_train()`](https://ahaeusser.github.io/tscv/reference/slice_train.md), [`slice_test()`](https://ahaeusser.github.io/tscv/reference/slice_test.md) | Create rolling-origin splits and extract training/test samples |
|  | [`make_future()`](https://ahaeusser.github.io/tscv/reference/make_future.md), [`make_errors()`](https://ahaeusser.github.io/tscv/reference/make_errors.md), [`make_accuracy()`](https://ahaeusser.github.io/tscv/reference/make_accuracy.md) | Convert forecasts and evaluate forecast accuracy |
|  | [`make_tsibble()`](https://ahaeusser.github.io/tscv/reference/make_tsibble.md) | Convert data to a `tsibble` using the package context |
| **Data summaries** | [`check_data()`](https://ahaeusser.github.io/tscv/reference/check_data.md), [`summarise_data()`](https://ahaeusser.github.io/tscv/reference/summarise_data.md), [`summarise_stats()`](https://ahaeusser.github.io/tscv/reference/summarise_stats.md) | Check and summarize time series data |
| **Autocorrelation analysis** | [`acf_vec()`](https://ahaeusser.github.io/tscv/reference/acf_vec.md), [`pacf_vec()`](https://ahaeusser.github.io/tscv/reference/pacf_vec.md), [`estimate_acf()`](https://ahaeusser.github.io/tscv/reference/estimate_acf.md), [`estimate_pacf()`](https://ahaeusser.github.io/tscv/reference/estimate_pacf.md) | Estimate ACF and PACF values |
| **Distributional statistics** | [`estimate_mode()`](https://ahaeusser.github.io/tscv/reference/estimate_mode.md), [`estimate_skewness()`](https://ahaeusser.github.io/tscv/reference/estimate_skewness.md), [`estimate_kurtosis()`](https://ahaeusser.github.io/tscv/reference/estimate_kurtosis.md) | Estimate descriptive distributional statistics |
| **Data preprocessing** | [`interpolate_missing()`](https://ahaeusser.github.io/tscv/reference/interpolate_missing.md), [`smooth_outlier()`](https://ahaeusser.github.io/tscv/reference/smooth_outlier.md) | Handle missing values and outliers |
| **Visualization** | [`plot_line()`](https://ahaeusser.github.io/tscv/reference/plot_line.md), [`plot_point()`](https://ahaeusser.github.io/tscv/reference/plot_point.md), [`plot_bar()`](https://ahaeusser.github.io/tscv/reference/plot_bar.md), [`plot_histogram()`](https://ahaeusser.github.io/tscv/reference/plot_histogram.md), [`plot_density()`](https://ahaeusser.github.io/tscv/reference/plot_density.md), [`plot_qq()`](https://ahaeusser.github.io/tscv/reference/plot_qq.md) | Create common exploratory plots |
| **Themes and scales** | [`theme_tscv()`](https://ahaeusser.github.io/tscv/reference/theme_tscv.md), [`theme_tscv_dark()`](https://ahaeusser.github.io/tscv/reference/theme_tscv_dark.md), [`scale_color_tscv()`](https://ahaeusser.github.io/tscv/reference/scale_color_tscv.md), [`scale_fill_tscv()`](https://ahaeusser.github.io/tscv/reference/scale_fill_tscv.md), [`tscv_cols()`](https://ahaeusser.github.io/tscv/reference/tscv_cols.md), [`tscv_pal()`](https://ahaeusser.github.io/tscv/reference/tscv_pal.md) | Apply `tscv` themes and colour palettes |
| **Benchmark models** | [`SMEAN()`](https://ahaeusser.github.io/tscv/reference/SMEAN.md), [`SMEDIAN()`](https://ahaeusser.github.io/tscv/reference/SMEDIAN.md), [`MEDIAN()`](https://ahaeusser.github.io/tscv/reference/MEDIAN.md), [`SNAIVE2()`](https://ahaeusser.github.io/tscv/reference/SNAIVE2.md) | Additional benchmark models compatible with `fable` |
| **Additional forecasting models** | [`DSHW()`](https://ahaeusser.github.io/tscv/reference/DSHW.md), [`TBATS()`](https://ahaeusser.github.io/tscv/reference/TBATS.md) | Additional forecasting models compatible with `fable` |

## Data sets

The package includes example data sets that can be used for testing,
examples and vignettes.

| **Data set** | **Description** |
|:---|:---|
| `elec_price` | Hourly day-ahead electricity spot prices for selected European bidding zones |
| `elec_load` | Electricity load data |
| `M4_monthly_data` | Selected monthly time series from the M4 forecasting competition |
| `M4_quarterly_data` | Selected quarterly time series from the M4 forecasting competition |

## Example

The following minimal example shows how to create time series
cross-validation splits for selected electricity price series.

``` r
library(tscv)
library(tidyverse)

context <- list(
  series_id = "bidding_zone",
  value_id = "value",
  index_id = "time"
)

main_frame <- elec_price %>%
  filter(bidding_zone %in% c("DE", "FR"))

split_frame <- make_split(
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

split_frame
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
```

The resulting object contains one row per time series and split. The
`train` and `test` columns are list-columns containing the row positions
used for training and testing.

## Vignettes

The package vignettes provide more detailed examples:

- fixed window time series cross-validation
- expanding window time series cross-validation
- visualization of time series data

## Links

- [Website](https://ahaeusser.github.io/tscv/)
- [GitHub](https://github.com/ahaeusser/tscv)
