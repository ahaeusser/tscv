# Summarise distributional statistics by time series

Calculate descriptive statistics for one or more time series.

## Usage

``` r
summarise_stats(.data, context)
```

## Arguments

- .data:

  A `tibble` in long format containing time series data.

- context:

  A named `list` with the identifiers for `series_id`, `value_id`, and
  `index_id`.

## Value

A `tibble` containing one row per time series and the calculated
descriptive statistics.

## Details

`summarise_stats()` groups the input data by the series identifier
supplied in `context` and returns one row per time series.

The function reports:

- `mean`: arithmetic mean;

- `median`: median;

- `mode`: kernel-density based mode estimate;

- `sd`: standard deviation;

- `p0`: minimum;

- `p25`: 25 percent quantile;

- `p75`: 75 percent quantile;

- `p100`: maximum;

- `skewness`: moment-based skewness;

- `kurtosis`: moment-based kurtosis.

Missing values are removed when calculating the statistics.

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
[`summarise_split()`](https://ahaeusser.github.io/tscv/reference/summarise_split.md)

## Examples

``` r
library(dplyr)

context <- list(
  series_id = "series",
  value_id = "value",
  index_id = "index"
)

data <- M4_monthly_data |>
  filter(series %in% c("M23100", "M14395"))

summarise_stats(
  .data = data,
  context = context
)
#> # A tibble: 2 × 11
#>   series  mean median  mode    sd    p0   p25   p75  p100 skewness kurtosis
#>   <chr>  <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
#> 1 M14395 1422.  1369. 1113.  490.  586. 1031. 1697.  3253    0.786     3.62
#> 2 M23100 9059.  9050  9040.  394. 8160  8768. 9310  10040    0.222     2.74
```
