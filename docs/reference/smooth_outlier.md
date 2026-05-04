# Identify and replace outliers

Identify outliers in a numeric time series and replace them with
smoothed values.

## Usage

``` r
smooth_outlier(x, periods, ...)
```

## Arguments

- x:

  Numeric vector containing the time series observations.

- periods:

  Numeric vector giving the seasonal periods of the time series, for
  example `12` for monthly data with yearly seasonality or `c(24, 168)`
  for hourly data with daily and weekly seasonality.

- ...:

  Further arguments passed to
  [`forecast::msts()`](https://pkg.robjhyndman.com/forecast/reference/msts.html)
  or
  [`forecast::tsoutliers()`](https://pkg.robjhyndman.com/forecast/reference/tsoutliers.html).

## Value

A numeric vector where detected outliers are replaced by smoothed
values.

## Details

`smooth_outlier()` is a small wrapper around
[`forecast::tsoutliers()`](https://pkg.robjhyndman.com/forecast/reference/tsoutliers.html).
The input vector is first converted to an `msts` object using the
seasonal periods supplied in `periods`.

For non-seasonal time series,
[`forecast::tsoutliers()`](https://pkg.robjhyndman.com/forecast/reference/tsoutliers.html)
uses a `supsmu`-based approach. For seasonal time series, the series is
decomposed using STL and outliers are identified on the remainder
component. Detected outliers are replaced by the replacement values
returned by
[`forecast::tsoutliers()`](https://pkg.robjhyndman.com/forecast/reference/tsoutliers.html).

The function returns a plain numeric vector with the same length as the
input.

## See also

Other data preparation:
[`check_data()`](https://ahaeusser.github.io/tscv/reference/check_data.md),
[`interpolate_missing()`](https://ahaeusser.github.io/tscv/reference/interpolate_missing.md)

## Examples

``` r
library(dplyr)

x <- M4_monthly_data |>
  filter(series == first(series)) |>
  pull(value)

x_outlier <- x
x_outlier[20] <- x_outlier[20] * 5

x_smoothed <- smooth_outlier(
  x = x_outlier,
  periods = 12
)

x_outlier[20]
#> [1] 8085.211
x_smoothed[20]
#> [1] 1629.107

hourly <- elec_price |>
  filter(bidding_zone == "DE") |>
  slice_head(n = 24 * 14) |>
  pull(value)

hourly_outlier <- hourly
hourly_outlier[48] <- hourly_outlier[48] * 5

smooth_outlier(
  x = hourly_outlier,
  periods = c(24, 168)
)
#>   [1]  10.07  -4.08  -9.91  -7.41 -12.55 -17.25 -15.07  -4.93  -6.33  -4.93
#>  [11]   0.45   0.12  -0.02   0.00  -0.03   1.97   9.06   0.07  -4.97  -6.98
#>  [21] -24.93  -4.87 -28.93 -33.57 -45.92 -48.29 -44.99 -37.45 -29.91  -0.01
#>  [31]  37.43  48.06  50.74  47.57  43.94  40.97  44.95  49.64  53.67  56.01
#>  [41]  56.95  62.08  62.11  57.99  55.64  55.13  50.76  47.99  45.22  45.63
#>  [51]  44.00  43.88  45.92  51.07  52.77  62.89  60.03  58.19  62.99  63.52
#>  [61]  64.67  65.24  67.76  68.41  69.55  67.28  69.46  68.38  61.72  53.72
#>  [71]  49.98  50.73  47.11  47.07  46.94  47.00  46.91  49.59  55.32  55.78
#>  [81]  55.52  55.23  53.58  51.74  51.60  51.41  51.69  52.59  54.66  54.10
#>  [91]  51.89  46.58  45.43  43.96  31.41  26.90  25.12  24.12  22.04  18.37
#> [101]  22.09  23.35  28.76  36.63  40.46  45.85  49.80  51.36  51.74  51.92
#> [111]  53.22  56.62  56.92  61.64  59.44  52.75  51.90  51.38  49.96  50.29
#> [121]  47.72  48.38  48.02  44.23  47.17  48.19  49.11  50.44  53.40  56.55
#> [131]  60.02  60.22  55.00  52.39  51.57  54.71  63.43  67.37  67.20  66.03
#> [141]  55.36  58.59  53.70  46.03  47.98  47.84  46.11  46.08  47.62  55.77
#> [151]  68.61  74.15  74.93  73.59  71.23  68.79  66.75  62.47  53.25  53.26
#> [161]  53.42  47.91  42.05  40.96  32.04  20.82   1.84  17.94  20.91   7.78
#> [171]  14.33  18.56  18.57  35.81  43.87  46.93  43.88  43.85  46.74  43.94
#> [181]  43.21  43.81  45.60  35.21  45.64  45.63  37.94  39.53  35.97  29.72
#> [191]  22.55  20.04   7.24   3.43  10.04  14.20  25.41  36.98  43.89  50.98
#> [201]  50.30  50.21  45.80  43.64  43.49  45.05  49.95  53.82  55.94  54.19
#> [211]  54.19  52.98  51.68  48.95  47.63  46.48  49.08  47.11  48.80  48.50
#> [221]  49.80  56.97  72.60  81.17  81.79  81.15  80.00  76.39  75.00  76.06
#> [231]  75.40  76.78  83.45  85.15  78.16  71.92  67.43  60.49  54.45  47.84
#> [241]  46.58  45.74  43.81  46.24  46.46  51.29  60.53  60.02  62.92  61.58
#> [251]  60.60  51.89  50.80  45.46  42.20  45.00  51.51  48.59  49.20  44.99
#> [261]  44.93  46.40  43.46  47.24  45.88  44.61  42.90  42.03  39.33  42.98
#> [271]  41.23  42.12  45.10  44.27  44.18  42.05  38.61  37.76  33.03  35.84
#> [281]  44.24  46.29  40.05  38.15  30.26  40.20  30.89  23.35  12.40  19.01
#> [291]  23.51  23.25  20.01  20.80  22.14  23.68  25.05  26.30  30.81  27.06
#> [301]   8.50  -0.08  -0.01  -0.27  21.55  24.04  15.91  -0.02  -0.33   3.04
#> [311]  -7.28  -3.14 -12.00 -15.04 -11.09  -4.94   1.59  35.90  51.22  51.66
#> [321]  46.09  50.26  44.49  41.24  38.91  41.79  49.63  50.93  51.25  50.93
#> [331]  50.52  49.61  44.30  41.29  37.26  35.18
```
