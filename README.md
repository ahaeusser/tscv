
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tscv

<!-- badges: start -->

<!-- badges: end -->

The package tscv provides a collection of functions and tools for time
series analysis and forecasting as well as time series cross-validation.
This is mainly a selection of functions that I find useful for research
in the area of time series forecasting.

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
```

The dataset `elec_price` is a hourly ‘tsibble’ with day-ahead
electricity spot prices from the ENTSO-E Transparency Platform. The
dataset contains time series data from 2019-01-01 00:00:00 to 2019-12-31
23:00:00 for five bidding zones in europe. We can use the function
`clean_data` to prepare the dataset for further usage. The function
checks whether the input data are a valid tsibble or not (regular spaced
in time and ordered). Furthermore, implicit missing values are turned
into explicit missing values (existing missing values are left
untouched). If the data are provided in wide format, they are gathered
into long format. For convenience, the index variable is renamed to
`date_time`, the key variable is renamed to `variable` and the
measurement variable is renamed to `value`.

``` r
data <- elec_price %>%
  filter(variable == "Day-ahead Price (DE)") %>%
  clean_data()

data
#> # A tsibble: 8,760 x 3 [1h] <UTC>
#> # Key:       variable [1]
#>    date_time           variable              value
#>    <dttm>              <chr>                 <dbl>
#>  1 2019-01-01 00:00:00 Day-ahead Price (DE)  10.1 
#>  2 2019-01-01 01:00:00 Day-ahead Price (DE)  -4.08
#>  3 2019-01-01 02:00:00 Day-ahead Price (DE)  -9.91
#>  4 2019-01-01 03:00:00 Day-ahead Price (DE)  -7.41
#>  5 2019-01-01 04:00:00 Day-ahead Price (DE) -12.6 
#>  6 2019-01-01 05:00:00 Day-ahead Price (DE) -17.2 
#>  7 2019-01-01 06:00:00 Day-ahead Price (DE) -15.1 
#>  8 2019-01-01 07:00:00 Day-ahead Price (DE)  -4.93
#>  9 2019-01-01 08:00:00 Day-ahead Price (DE)  -6.33
#> 10 2019-01-01 09:00:00 Day-ahead Price (DE)  -4.93
#> # ... with 8,750 more rows

plot_ts(data)
```

<img src="man/figures/README-clean data-1.png" width="100%" />

To prepare the dataset for time series cross-validation, we can use the
function `split_data`. This function splits the data into training and
testing (i.e. partitioning into time slices) for time series
cross-validation. The user can choose between `stretch` and `slide`. The
first is an expanding window approach, while the latter is a fixed
window approach. The user can define the window sizes for training and
testing via `n_init` and `n_ahead`, as well as the step size for
increments via `n_step`.

``` r

mode <- "slide"
n_init <- 2400
n_step <- 24
n_ahead <- 24

data <- data %>%
  split_data(
    mode = mode,
    n_init = n_init,
    n_step = n_step,
    n_ahead = n_ahead)

# Fur illutrative purpose, we only use the first five time slices ...
data <- data %>%
  filter(slice %in% c(1:5))

data
#> # A tsibble: 12,120 x 8 [1h] <UTC>
#> # Key:       variable, slice [5]
#>    date_time           variable          sample slice horizon type  model  value
#>    <dttm>              <chr>             <chr>  <int>   <int> <chr> <chr>  <dbl>
#>  1 2019-01-01 00:00:00 Day-ahead Price ~ train      1      NA actu~ <NA>   10.1 
#>  2 2019-01-01 01:00:00 Day-ahead Price ~ train      1      NA actu~ <NA>   -4.08
#>  3 2019-01-01 02:00:00 Day-ahead Price ~ train      1      NA actu~ <NA>   -9.91
#>  4 2019-01-01 03:00:00 Day-ahead Price ~ train      1      NA actu~ <NA>   -7.41
#>  5 2019-01-01 04:00:00 Day-ahead Price ~ train      1      NA actu~ <NA>  -12.6 
#>  6 2019-01-01 05:00:00 Day-ahead Price ~ train      1      NA actu~ <NA>  -17.2 
#>  7 2019-01-01 06:00:00 Day-ahead Price ~ train      1      NA actu~ <NA>  -15.1 
#>  8 2019-01-01 07:00:00 Day-ahead Price ~ train      1      NA actu~ <NA>   -4.93
#>  9 2019-01-01 08:00:00 Day-ahead Price ~ train      1      NA actu~ <NA>   -6.33
#> 10 2019-01-01 09:00:00 Day-ahead Price ~ train      1      NA actu~ <NA>   -4.93
#> # ... with 12,110 more rows
```

``` r
models <- data %>%
  filter(sample == "train") %>%
  model(
    sNaive = SNAIVE(value),
    ARIMA = ARIMA(value ~ fourier(period = c(24, 168), K = c(6, 6)) + PDQ(0,0,0)))

fcsts <- models %>%
  forecast_model(n_ahead = n_ahead)
```

``` r
data <- bind_data(
  data = data,
  fcsts = fcsts)

data
#> # A tsibble: 12,360 x 8 [1h] <UTC>
#> # Key:       variable, slice, model, type [15]
#>    date_time           variable           sample slice horizon type  model value
#>    <dttm>              <chr>              <chr>  <int>   <int> <chr> <chr> <dbl>
#>  1 2019-04-11 00:00:00 Day-ahead Price (~ test       1       1 fcst  ARIMA  35.5
#>  2 2019-04-11 01:00:00 Day-ahead Price (~ test       1       2 fcst  ARIMA  35.7
#>  3 2019-04-11 02:00:00 Day-ahead Price (~ test       1       3 fcst  ARIMA  35.6
#>  4 2019-04-11 03:00:00 Day-ahead Price (~ test       1       4 fcst  ARIMA  36.1
#>  5 2019-04-11 04:00:00 Day-ahead Price (~ test       1       5 fcst  ARIMA  40.3
#>  6 2019-04-11 05:00:00 Day-ahead Price (~ test       1       6 fcst  ARIMA  48.0
#>  7 2019-04-11 06:00:00 Day-ahead Price (~ test       1       7 fcst  ARIMA  54.7
#>  8 2019-04-11 07:00:00 Day-ahead Price (~ test       1       8 fcst  ARIMA  56.7
#>  9 2019-04-11 08:00:00 Day-ahead Price (~ test       1       9 fcst  ARIMA  55.4
#> 10 2019-04-11 09:00:00 Day-ahead Price (~ test       1      10 fcst  ARIMA  53.6
#> # ... with 12,350 more rows
```

``` r
variable <- c("Day-ahead Price (DE)")
model <- c("sNaive", "ARIMA")
slice <- c(1)

plot_forecast(
  data = data,
  variable = variable,
  model = model,
  slice = slice,
  title = "Day-ahead Price (DE)",
  subtitle = "Forecast from 2019-04-11 00:00 to 2019-04-11 23:00",
  caption = "Data: ENTSO-E Transparency Platform",
  ylab = "[Euro/MWh]")
```

<img src="man/figures/README-plot_forecast-1.png" width="100%" />

``` r
# Evaluation
errors <- data %>%
  create_errors()

errors
#> # A tsibble: 480 x 8 [1h] <UTC>
#> # Key:       variable, slice, model, type [20]
#>    date_time           variable          sample slice horizon type  model  value
#>    <dttm>              <chr>             <chr>  <int>   <int> <chr> <chr>  <dbl>
#>  1 2019-04-11 00:00:00 Day-ahead Price ~ test       1       1 error ARIMA  1.52 
#>  2 2019-04-11 01:00:00 Day-ahead Price ~ test       1       2 error ARIMA  0.869
#>  3 2019-04-11 02:00:00 Day-ahead Price ~ test       1       3 error ARIMA  1.50 
#>  4 2019-04-11 03:00:00 Day-ahead Price ~ test       1       4 error ARIMA  2.75 
#>  5 2019-04-11 04:00:00 Day-ahead Price ~ test       1       5 error ARIMA  7.61 
#>  6 2019-04-11 05:00:00 Day-ahead Price ~ test       1       6 error ARIMA  8.37 
#>  7 2019-04-11 06:00:00 Day-ahead Price ~ test       1       7 error ARIMA  4.57 
#>  8 2019-04-11 07:00:00 Day-ahead Price ~ test       1       8 error ARIMA -5.01 
#>  9 2019-04-11 08:00:00 Day-ahead Price ~ test       1       9 error ARIMA -6.86 
#> 10 2019-04-11 09:00:00 Day-ahead Price ~ test       1      10 error ARIMA -7.62 
#> # ... with 470 more rows

plot_error_dist(
  data = errors,
  variable = variable,
  model = model)
```

<img src="man/figures/README-errors-1.png" width="100%" />

``` r
metrics <- errors %>%
  error_metrics()

metrics
#> # A tibble: 336 x 7
#>    variable             model  dim     num type  metric   value
#>    <chr>                <chr>  <chr> <int> <chr> <chr>    <dbl>
#>  1 Day-ahead Price (DE) ARIMA  slice     1 test  ME     -3.49  
#>  2 Day-ahead Price (DE) ARIMA  slice     2 test  ME     -0.0646
#>  3 Day-ahead Price (DE) ARIMA  slice     3 test  ME      0.531 
#>  4 Day-ahead Price (DE) ARIMA  slice     4 test  ME      1.00  
#>  5 Day-ahead Price (DE) sNaive slice     1 test  ME      2.10  
#>  6 Day-ahead Price (DE) sNaive slice     2 test  ME      0.397 
#>  7 Day-ahead Price (DE) sNaive slice     3 test  ME     -4.57  
#>  8 Day-ahead Price (DE) sNaive slice     4 test  ME     -3.45  
#>  9 Day-ahead Price (DE) ARIMA  slice     1 test  MAE     5.76  
#> 10 Day-ahead Price (DE) ARIMA  slice     2 test  MAE     5.35  
#> # ... with 326 more rows

metric <- c("MAE", "MAPE")

plot_error_metrics(
  data = metrics,
  variable = variable,
  model = model,
  metric = metric)
```

<img src="man/figures/README-metrics-1.png" width="100%" />
