# Test a time series for seasonality

Tests whether a numeric time series exhibits seasonality at a specified
frequency. The test compares the absolute autocorrelation at the
seasonal lag with a 90 percent critical limit. At least three complete
seasonal cycles are required. Series with a frequency of one are treated
as non-seasonal.

## Usage

``` r
test_seasonality(x, freq)
```

## Arguments

- x:

  A numeric vector containing the observed time series.

- freq:

  A positive whole number specifying the seasonal frequency, such as
  \`12\` for monthly data or \`4\` for quarterly data.

## Value

A single logical value. \`TRUE\` indicates that seasonality was
detected; otherwise, \`FALSE\`.

## Examples

``` r
x <- as.numeric(AirPassengers)

test_seasonality(x, freq = 12)
#> [1] TRUE
```
