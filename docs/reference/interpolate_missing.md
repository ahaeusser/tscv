# Interpolate missing values

Interpolate missing values in a numeric time series.

## Usage

``` r
interpolate_missing(x, periods, ...)
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
  [`forecast::na.interp()`](https://pkg.robjhyndman.com/forecast/reference/na.interp.html).

## Value

A numeric vector with missing values interpolated.

## Details

`interpolate_missing()` is a small wrapper around
[`forecast::na.interp()`](https://pkg.robjhyndman.com/forecast/reference/na.interp.html).
The input vector is first converted to an `msts` object using the
seasonal periods supplied in `periods`.

For non-seasonal time series, missing values are replaced using linear
interpolation. For seasonal time series,
[`forecast::na.interp()`](https://pkg.robjhyndman.com/forecast/reference/na.interp.html)
uses an STL-based approach: the series is decomposed, the seasonally
adjusted series is interpolated, and the seasonal component is added
back.

The function returns a plain numeric vector with the same length as the
input.

## See also

Other data preparation:
[`check_data()`](https://ahaeusser.github.io/tscv/reference/check_data.md),
[`smooth_outlier()`](https://ahaeusser.github.io/tscv/reference/smooth_outlier.md)

## Examples

``` r
library(dplyr)

x <- M4_monthly_data |>
  filter(series == first(series)) |>
  pull(value)

x_missing <- x
x_missing[c(10, 20, 30)] <- NA

x_interpolated <- interpolate_missing(
  x = x_missing,
  periods = 12
)

anyNA(x_missing)
#> [1] TRUE
anyNA(x_interpolated)
#> [1] FALSE

hourly <- elec_price |>
  filter(bidding_zone == "DE") |>
  slice_head(n = 24 * 14) |>
  pull(value)

hourly_missing <- hourly
hourly_missing[c(24, 48, 72)] <- NA

interpolate_missing(
  x = hourly_missing,
  periods = c(24, 168)
)
#>   [1]  10.070  -4.080  -9.910  -7.410 -12.550 -17.250 -15.070  -4.930  -6.330
#>  [10]  -4.930   0.450   0.120  -0.020   0.000  -0.030   1.970   9.060   0.070
#>  [19]  -4.970  -6.980 -24.930  -4.870 -28.930 -37.425 -45.920 -48.290 -44.990
#>  [28] -48.930 -29.910  -0.010  37.430  48.060  50.740  47.570  43.940  40.970
#>  [37]  44.950  49.640  53.670  56.010  56.950  62.080  62.110  57.990  55.640
#>  [46]  55.130  50.760  47.990  45.220  45.630  44.000  43.880  45.920  51.070
#>  [55]  52.770  62.890  60.030  58.190  62.990  63.520  64.670  65.240  67.760
#>  [64]  68.410  69.550  67.280  69.460  68.380  61.720  53.720  49.980  48.545
#>  [73]  47.110  47.070  46.940  47.000  46.910  49.590  55.320  55.780  55.520
#>  [82]  55.230  53.580  51.740  51.600  51.410  51.690  52.590  54.660  54.100
#>  [91]  51.890  46.580  45.430  43.960  31.410  26.900  25.120  24.120  22.040
#> [100]  18.370  22.090  23.350  28.760  36.630  40.460  45.850  49.800  51.360
#> [109]  51.740  51.920  53.220  56.620  56.920  61.640  59.440  52.750  51.900
#> [118]  51.380  49.960  50.290  47.720  48.380  48.020  44.230  47.170  48.190
#> [127]  49.110  50.440  53.400  56.550  60.020  60.220  55.000  52.390  51.570
#> [136]  54.710  63.430  67.370  67.200  66.030  55.360  58.590  53.700  46.030
#> [145]  47.980  47.840  46.110  46.080  47.620  55.770  68.610  74.150  74.930
#> [154]  73.590  71.230  68.790  66.750  62.470  53.250  53.260  53.420  47.910
#> [163]  42.050  40.960  32.040  20.820   1.840  17.940  20.910   7.780  14.330
#> [172]  18.560  18.570  35.810  43.870  46.930  43.880  43.850  46.740  43.940
#> [181]  43.210  43.810  45.600  35.210  45.640  45.630  37.940  39.530  35.970
#> [190]  29.720  22.550  20.040   7.240   3.430  10.040  14.200  25.410  36.980
#> [199]  43.890  50.980  50.300  50.210  45.800  43.640  43.490  45.050  49.950
#> [208]  53.820  55.940  54.190  54.190  52.980  51.680  48.950  47.630  46.480
#> [217]  49.080  47.110  48.800  48.500  49.800  56.970  72.600  81.170  81.790
#> [226]  81.150  80.000  76.390  75.000  76.060  75.400  76.780  83.450  85.150
#> [235]  78.160  71.920  67.430  60.490  54.450  47.840  46.580  45.740  43.810
#> [244]  46.240  46.460  51.290  60.530  60.020  62.920  61.580  60.600  51.890
#> [253]  50.800  45.460  42.200  45.000  51.510  48.590  49.200  44.990  44.930
#> [262]  46.400  43.460  47.240  45.880  44.610  42.900  42.030  39.330  42.980
#> [271]  41.230  42.120  45.100  44.270  44.180  42.050  38.610  37.760  33.030
#> [280]  35.840  44.240  46.290  40.050  38.150  30.260  40.200  30.890  23.350
#> [289]  12.400  19.010  23.510  23.250  20.010  20.800  22.140  23.680  25.050
#> [298]  26.300  30.810  27.060   8.500  -0.080  -0.010  -0.270  21.550  24.040
#> [307]  15.910  -0.020  -0.330   3.040  -7.280  -3.140 -12.000 -15.040 -11.090
#> [316]  -4.940   1.590  35.900  51.220  51.660  46.090  50.260  44.490  41.240
#> [325]  38.910  41.790  49.630  50.930  51.250  50.930  50.520  49.610  44.300
#> [334]  41.290  37.260  35.180
```
