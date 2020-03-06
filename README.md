
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tscv

<!-- badges: start -->

<!-- badges: end -->

The package tscv provides a collection of functions and utilities for
time series cross validation and tidy time series forecasting based on
the tidyverts (tsibble, fable and fabletools).

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ahaeusser/tscv")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# Load relevant packages
library(tscv)
library(tidyverse)
library(tsibble)
library(fable)
```

The dataset `elec_price` is a hourly “tsibble” with day-ahead
electricity spot prices from the ENTSO-E Transparency Platform. The
dataset contains time series data from 2019-01-01 00:00:00 to 2019-12-31
23:00:00 for five bidding zones within europe. We can use the function
`clean_data` to prepare the dataset for further usage:

``` r
data <- elec_price %>% 
  clean_data()

data
#> # A tsibble: 43,800 x 3 [1h] <UTC>
#> # Key:       variable [5]
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
#> # ... with 43,790 more rows

plot_ts(data)
```

<img src="man/figures/README-clean data-1.png" width="100%" />

To prepare the dataset for time series cross-validation, we can use the
function `split_data`:

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
    n_ahead = n_ahead
    )

data
#> # A tsibble: 3,223,920 x 8 [1h] <UTC>
#> # Key:       variable, slice [1,330]
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
#> # ... with 3,223,910 more rows
```
