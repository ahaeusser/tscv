
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tscv

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The package `tscv` provides a collection of functions and tools for time
series analysis and forecasting as well as time series cross-validation.
This is mainly a set of wrapper and helper functions as well as some
extensions for the packages `tsibble`, `fable` and `fabletools` that I
find useful for research in the area of time series forecasting.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ahaeusser/tscv")
```

## Example

``` r
# Load relevant packages
library(tscv)
library(tidyverse)
library(tsibble)
library(fable)
library(feasts)
Sys.setlocale("LC_TIME", "C")
#> [1] "C"
```

### Data preparation

The dataset `elec_price` is a hourly `tsibble` with day-ahead
electricity spot prices from the ENTSO-E Transparency Platform. The
dataset contains time series data from 2019-01-01 00:00:00 to 2019-12-31
23:00:00 for four european bidding zones (DE, FR, NO1 and SE1). You can
use the function `clean_data()` to prepare the dataset for further
usage. The function checks whether the input data are a valid tsibble or
not (regular spaced in time and ordered). Furthermore, implicit missing
values are turned into explicit missing values (existing missing values
are left untouched). If the data are provided in wide format, they are
gathered into long format. You can use the function `plot_line()` to
visualize the four time series.

``` r
data <- elec_price %>%
  mutate(Series = paste0(Series, " (", BZN, ")")) %>%
  update_tsibble(key = Series) %>%
  select(-c(Unit, BZN)) %>%
  clean_data()

data
#> # A tsibble: 35,040 x 3 [1h] <UTC>
#> # Key:       Series [4]
#>    Time                Series         Value
#>    <dttm>              <chr>          <dbl>
#>  1 2019-01-01 00:00:00 Price (DE-LU)  10.1 
#>  2 2019-01-01 01:00:00 Price (DE-LU)  -4.08
#>  3 2019-01-01 02:00:00 Price (DE-LU)  -9.91
#>  4 2019-01-01 03:00:00 Price (DE-LU)  -7.41
#>  5 2019-01-01 04:00:00 Price (DE-LU) -12.6 
#>  6 2019-01-01 05:00:00 Price (DE-LU) -17.2 
#>  7 2019-01-01 06:00:00 Price (DE-LU) -15.1 
#>  8 2019-01-01 07:00:00 Price (DE-LU)  -4.93
#>  9 2019-01-01 08:00:00 Price (DE-LU)  -6.33
#> 10 2019-01-01 09:00:00 Price (DE-LU)  -4.93
#> # ... with 35,030 more rows

data %>%
  plot_line(
    title = "Day-ahead Electricity Spot Price",
    subtitle = "2019-01-01 to 2019-12-31",
    xlab = "Time",
    ylab = "[EUR/MWh]",
    caption = "Data: ENTSO-E Transparency")
```

<img src="man/figures/README-clean data-1.svg" width="100%" />

To prepare the dataset for time series cross-validation (TSCV), you can
use the function `split_data()`. This function splits the data into
training and testing (i.e. partitioning into time slices) for time
series cross-validation. You can choose between `stretch` and `slide`.
The first is an expanding window approach, while the latter is a fixed
window approach. Furthermore, you can define the (initial) window size
for training and testing via `n_init` and `n_ahead`, as well as the step
size for increments via `n_skip`.

``` r

n_init <- 2400   # size for training window
n_ahead <- 24    # size for testing window (forecast horizon)
mode <- "slide"  # fixed window approach
n_skip <- 23     # skip 23 observations
n_lag <- 0       # no lag

data <- data %>%
  split_data(
    n_init = n_init,
    n_ahead = n_ahead,
    mode = mode,
    n_skip = n_skip,
    n_lag = n_lag)

data
#> # A tsibble: 2,579,136 x 7 [1h] <UTC>
#> # Key:       Series, split [1,064]
#>    Time                Series         Value split    id sample horizon
#>    <dttm>              <chr>          <dbl> <int> <int> <chr>    <int>
#>  1 2019-01-01 00:00:00 Price (DE-LU)  10.1      1     1 train       NA
#>  2 2019-01-01 01:00:00 Price (DE-LU)  -4.08     1     2 train       NA
#>  3 2019-01-01 02:00:00 Price (DE-LU)  -9.91     1     3 train       NA
#>  4 2019-01-01 03:00:00 Price (DE-LU)  -7.41     1     4 train       NA
#>  5 2019-01-01 04:00:00 Price (DE-LU) -12.6      1     5 train       NA
#>  6 2019-01-01 05:00:00 Price (DE-LU) -17.2      1     6 train       NA
#>  7 2019-01-01 06:00:00 Price (DE-LU) -15.1      1     7 train       NA
#>  8 2019-01-01 07:00:00 Price (DE-LU)  -4.93     1     8 train       NA
#>  9 2019-01-01 08:00:00 Price (DE-LU)  -6.33     1     9 train       NA
#> 10 2019-01-01 09:00:00 Price (DE-LU)  -4.93     1    10 train       NA
#> # ... with 2,579,126 more rows
```

### Training and forecasting

Now the data are splitted into training and testing slices and we are
ready to forecast. Due to the sample size and computation time, only
very simple benchmark methods are used. The functions `SMEDIAN` and
`SMEAN` are extensions to the `fable` package. The function`SMEAN` is
exactly the same as running a regression against seasonal dummy
variables (`TSLM(value ~ season())`). I just added this function for
convenience. Further forecasting methods are available (e.g. `TBATS()`
and `DSHW()` from package `forecast` or `ELM()` and `MLP()` from package
`nnfor`).

``` r

data <- data %>%
  filter(split %in% c(1:100))

models <- data %>%
  filter(sample == "train") %>%
  model(
    sNaive = SNAIVE(Value ~ lag("week")),
    sMean = SMEAN(Value ~ lag("week")),
    sMedian = SMEDIAN(Value ~ lag("week")),
    "STL-Naive" = decomposition_model(STL(Value), NAIVE(season_adjust)))

models
#> # A mable: 400 x 6
#> # Key:     Series, split [400]
#>    Series        split sNaive   sMean   sMedian   `STL-Naive`              
#>    <chr>         <int> <model>  <model> <model>   <model>                  
#>  1 Price (DE-LU)     1 <SNAIVE> <SMEAN> <SMEDIAN> <STL decomposition model>
#>  2 Price (DE-LU)     2 <SNAIVE> <SMEAN> <SMEDIAN> <STL decomposition model>
#>  3 Price (DE-LU)     3 <SNAIVE> <SMEAN> <SMEDIAN> <STL decomposition model>
#>  4 Price (DE-LU)     4 <SNAIVE> <SMEAN> <SMEDIAN> <STL decomposition model>
#>  5 Price (DE-LU)     5 <SNAIVE> <SMEAN> <SMEDIAN> <STL decomposition model>
#>  6 Price (DE-LU)     6 <SNAIVE> <SMEAN> <SMEDIAN> <STL decomposition model>
#>  7 Price (DE-LU)     7 <SNAIVE> <SMEAN> <SMEDIAN> <STL decomposition model>
#>  8 Price (DE-LU)     8 <SNAIVE> <SMEAN> <SMEDIAN> <STL decomposition model>
#>  9 Price (DE-LU)     9 <SNAIVE> <SMEAN> <SMEDIAN> <STL decomposition model>
#> 10 Price (DE-LU)    10 <SNAIVE> <SMEAN> <SMEDIAN> <STL decomposition model>
#> # ... with 390 more rows


fcst <- models %>%
  forecast(h = n_ahead)

fcst
#> # A fable: 38,400 x 6 [1h] <UTC>
#> # Key:     Series, split, .model [1,600]
#>    Series        split .model Time                Value .distribution
#>    <chr>         <int> <chr>  <dttm>              <dbl> <dist>       
#>  1 Price (DE-LU)     1 sNaive 2019-04-11 00:00:00  33   N(33, 367)   
#>  2 Price (DE-LU)     1 sNaive 2019-04-11 01:00:00  32.6 N(33, 367)   
#>  3 Price (DE-LU)     1 sNaive 2019-04-11 02:00:00  34.1 N(34, 367)   
#>  4 Price (DE-LU)     1 sNaive 2019-04-11 03:00:00  36.9 N(37, 367)   
#>  5 Price (DE-LU)     1 sNaive 2019-04-11 04:00:00  44.7 N(45, 367)   
#>  6 Price (DE-LU)     1 sNaive 2019-04-11 05:00:00  53.6 N(54, 367)   
#>  7 Price (DE-LU)     1 sNaive 2019-04-11 06:00:00  59.9 N(60, 367)   
#>  8 Price (DE-LU)     1 sNaive 2019-04-11 07:00:00  46.9 N(47, 367)   
#>  9 Price (DE-LU)     1 sNaive 2019-04-11 08:00:00  48   N(48, 367)   
#> 10 Price (DE-LU)     1 sNaive 2019-04-11 09:00:00  47   N(47, 367)   
#> # ... with 38,390 more rows

plot_forecast(
  fcst = fcst,
  data = data,
  include = 48,
  split = c(10, 11),
  title = "Day-ahead electricity spot price forecast",
  subtitle = "Rolling forecasts for splits 10 and 11",
  ylab = "[EUR/MWh]",
  caption = "Data: ENTSO-E Transparency, own calculation"
  )
```

<img src="man/figures/README-train-1.svg" width="100%" />

### Evaluation of forecast accuracy

To evaluate the forecast accuracy, the function `error_metrics()` is
used. You can define whether to evaluate the accuracy by `horizon` or by
`split`. Several accuracy metrics like `RMSE`, `MAE` or `MAPE` are
available.

``` r

# Estimate error metrics
metrics_horizon <- error_metrics(
  fcst = fcst,
  data = data,
  period = 168,
  by = "horizon")

metrics_horizon %>%
  plot_error_metrics(
    title = "Evaluation of forecast accuracy by forecast horizon",
    subtitle = "Seasonal mean absolute scaled error (sMASE)",
    xlab = "Forecast horizon (n-step-ahead)",
    caption = "Data: ENTSO-E Transparency, own calculation")
```

<img src="man/figures/README-accuracy-1.svg" width="100%" />

``` r

# Visualize results
metrics_split <- error_metrics(
  fcst = fcst,
  data = data,
  period = 168,
  by = "split")

metrics_split %>%
  plot_error_metrics(
    title = "Evaluation of forecast accuracy by split",
    subtitle = "Seasonal mean absolute scaled error (sMASE)",
    xlab = "Split",
    caption = "Data: ENTSO-E Transparency, own calculation")
```

<img src="man/figures/README-accuracy-2.svg" width="100%" />

### Work in progress
