# Check and prepare tsibble data

Check that the input data is a valid regular and ordered `tsibble`, fill
implicit gaps if requested, and convert wide data to long format.

## Usage

``` r
check_data(data, fill_missing = TRUE)
```

## Arguments

- data:

  A `tsibble` in long or wide format.

- fill_missing:

  Logical value. If `TRUE`, implicit missing values are turned into
  explicit missing values with
  [`fill_gaps()`](https://tsibble.tidyverts.org/reference/fill_gaps.html).

## Value

A `tsibble` prepared for downstream use. Wide data is returned in long
format with one measurement variable named `value`.

## Details

`check_data()` is a data preparation helper for time series workflows.
It performs three tasks:

- checks that `data` is a `tsibble`;

- checks that the time index is regular and ordered by key and index;

- optionally turns implicit missing values into explicit missing values
  using
  [`fill_gaps()`](https://tsibble.tidyverts.org/reference/fill_gaps.html).

If the input data has no key variables, it is treated as wide data and
is converted to long format. The resulting output contains the original
index column, a `variable` column containing the former column names,
and a `value` column containing the corresponding observations.

Existing explicit missing values are not changed.

## See also

Other data preparation:
[`interpolate_missing()`](https://ahaeusser.github.io/tscv/reference/interpolate_missing.md),
[`smooth_outlier()`](https://ahaeusser.github.io/tscv/reference/smooth_outlier.md)

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(tsibble)
#> 
#> Attaching package: 'tsibble'
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, union

data <- M4_monthly_data |>
  filter(series %in% c("M23100", "M14395")) |>
  as_tsibble(
    index = index,
    key = series
  )

check_data(data)
#> # A tsibble: 372 x 4 [1M]
#> # Key:       series [2]
#>       index series category value
#>       <mth> <chr>  <chr>    <dbl>
#>  1 2001 Jul M14395 Micro    1116.
#>  2 2001 Aug M14395 Micro    1079.
#>  3 2001 Sep M14395 Micro     917.
#>  4 2001 Okt M14395 Micro     982.
#>  5 2001 Nov M14395 Micro     946.
#>  6 2001 Dez M14395 Micro     586.
#>  7 2002 Jan M14395 Micro     710.
#>  8 2002 Feb M14395 Micro     714.
#>  9 2002 Mrz M14395 Micro     675 
#> 10 2002 Apr M14395 Micro    1068.
#> # ℹ 362 more rows

wide_data <- data |>
  as_tibble() |>
  select(index, series, value) |>
  tidyr::pivot_wider(
    names_from = series,
    values_from = value
  ) |>
  as_tsibble(index = index)

check_data(wide_data)
#> # A tsibble: 372 x 3 [1M]
#> # Key:       variable [2]
#>       index variable value
#>       <mth> <chr>    <dbl>
#>  1 2001 Jul M14395   1116.
#>  2 2001 Jul M23100     NA 
#>  3 2001 Aug M14395   1079.
#>  4 2001 Aug M23100     NA 
#>  5 2001 Sep M14395    917.
#>  6 2001 Sep M23100     NA 
#>  7 2001 Okt M14395    982.
#>  8 2001 Okt M23100     NA 
#>  9 2001 Nov M14395    946.
#> 10 2001 Nov M23100     NA 
#> # ℹ 362 more rows
```
