
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tscv <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![License](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![R-CMD-check](https://github.com/ahaeusser/tscv/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ahaeusser/tscv/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/ahaeusser/tscv/graph/badge.svg)](https://app.codecov.io/gh/ahaeusser/tscv)
<!-- badges: end -->

The package `tscv` provides helper functions for **time series
analysis**, **forecasting**, and **time series cross-validation**. It is
designed to work with the **tidy forecasting ecosystem**, especially
`tsibble`, `fable`, `fabletools`, and `feasts`.

The package contains tools for:

- creating **rolling-origin** resampling schemes for time series
  cross-validation
- slicing **training and test samples** from time-indexed data
- converting forecasts into a common format for evaluation
- calculating **forecast accuracy measures**
- **visualizing time series data**, forecast errors, and distributional
  properties
- fitting **additional benchmark and forecasting models** compatible
  with `fable`
- working with example time series data sets

The main focus of the package is to simplify repeated forecasting
experiments across several time series, models, forecast horizons, and
rolling-origin splits.

## Installation

You can install the development version from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("ahaeusser/tscv")
```

## Workflow

A typical workflow with `tscv` consists of the following steps:

1.  Prepare the data in long format.
2.  Define a `context` object identifying the series, value, and index
    columns.
3.  Create rolling-origin splits using `make_split()`.
4.  Slice the training and test data using `slice_train()` and
    `slice_test()`.
5.  Estimate forecasting models using `fable` or the additional models
    provided by `tscv`.
6.  Convert forecasts with `make_future()`.
7.  Evaluate forecast accuracy with `make_accuracy()`.
8.  Visualize the data, forecasts, or accuracy measures.

## Example

The central idea of time series cross-validation is to evaluate
forecasts repeatedly over time. Instead of relying on a single
train-test split, `tscv` creates several rolling-origin splits. Each
split contains a training window for model estimation and a test window
for forecast evaluation.

Two common rolling-origin schemes are **fixed window** and **expanding
window** cross-validation.

### Fixed window cross-validation

In a fixed window setup, both the training window and the test window
move forward through time. The length of the training sample stays
constant. This is useful when recent observations are expected to be
more informative than older observations, for example when the
data-generating process changes over time.

In the plot below, each row represents one split and each square
represents one time point. Blue squares are used for training, dark
squares are used for testing, and light squares are not used in that
split.

<div class="figure">

<img src="man/figures/README-fixed-window-plot-1.svg" alt="Fixed window time series cross-validation scheme" width="100%" />
<p class="caption">

Fixed window cross-validation: the training and test windows both move
forward through time.
</p>

</div>

For example, the following code creates fixed window splits for two
electricity price series. The first training window contains 2,400
observations and each test window contains the next 24 observations.
Since the origin itself advances by one step, `n_skip = 23` moves the
next split forward by 24 observations in total.

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
```

The resulting object contains one row per time series and split. The
`train` and `test` columns are list-columns with the row positions used
for model estimation and forecast evaluation.

### Expanding window cross-validation

In an expanding window setup, the start of the training sample stays
fixed while the end moves forward through time. The training sample
therefore grows with each split. This is useful when all historical
observations are considered informative and the goal is to mimic a
forecasting process where more data becomes available over time.

<div class="figure">

<img src="man/figures/README-expanding-window-plot-1.svg" alt="Expanding window time series cross-validation scheme" width="100%" />
<p class="caption">

Expanding window cross-validation: the training window grows while the
test window moves forward.
</p>

</div>

The same setup can be changed to expanding window cross-validation by
using `mode = "stretch"`.

``` r
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

Both approaches use the same forecast horizon and rolling-origin
structure. The difference is how the training sample is updated:

| Approach | Training window | Typical use case |
|:---|:---|:---|
| Fixed window | Moves forward with constant length | Recent observations are most relevant |
| Expanding window | Starts at the same point and grows over time | All historical observations remain useful |

After the splits have been created, they can be passed to the remaining
`tscv` workflow: slice the training and test samples, estimate
forecasting models, convert forecasts, and evaluate forecast accuracy.

## Function overview

The following table summarizes the main functions in `tscv` by topic.

| **Topic** | **Function(s)** | **Description** |
|:---|:---|:---|
| **Time series cross-validation** | `make_split()`, `slice_train()`, `slice_test()` | Create rolling-origin splits and extract training/test samples |
|  | `make_future()`, `make_errors()`, `make_accuracy()` | Convert forecasts and evaluate forecast accuracy |
|  | `make_tsibble()` | Convert data to a `tsibble` using the package context |
| **Data summaries** | `check_data()`, `summarise_data()`, `summarise_stats()` | Check and summarize time series data |
| **Autocorrelation analysis** | `acf_vec()`, `pacf_vec()`, `estimate_acf()`, `estimate_pacf()` | Estimate ACF and PACF values |
| **Distributional statistics** | `estimate_mode()`, `estimate_skewness()`, `estimate_kurtosis()` | Estimate descriptive distributional statistics |
| **Data preprocessing** | `interpolate_missing()`, `smooth_outlier()` | Handle missing values and outliers |
| **Visualization** | `plot_line()`, `plot_point()`, `plot_bar()`, `plot_histogram()`, `plot_density()`, `plot_qq()` | Create common exploratory plots |
| **Themes and scales** | `theme_tscv()`, `theme_tscv_dark()`, `scale_color_tscv()`, `scale_fill_tscv()`, `tscv_cols()`, `tscv_pal()` | Apply `tscv` themes and colour palettes |
| **Benchmark models** | `SMEAN()`, `SMEDIAN()`, `MEDIAN()`, `SNAIVE2()` | Additional benchmark models compatible with `fable` |
| **Additional forecasting models** | `DSHW()`, `TBATS()` | Additional forecasting models compatible with `fable` |

## Data sets

The package includes example data sets that can be used for testing,
examples, and vignettes.

| **Data set** | **Description** |
|:---|:---|
| `elec_price` | Hourly day-ahead electricity spot prices for selected European bidding zones |
| `elec_load` | Electricity load data |
| `M4_monthly_data` | Selected monthly time series from the M4 forecasting competition |
| `M4_quarterly_data` | Selected quarterly time series from the M4 forecasting competition |

## Vignettes

The package vignettes provide more detailed examples:

- fixed window time series cross-validation
- expanding window time series cross-validation
- visualization of time series data

## Links

- [Website](https://ahaeusser.github.io/tscv/)
- [GitHub](https://github.com/ahaeusser/tscv)
