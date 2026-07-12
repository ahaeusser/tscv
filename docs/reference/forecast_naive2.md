# Forecast a time series using the Naive2 method

Produces forecasts using the Naive2 benchmark from the M4 forecasting
competition. The function first applies
[`test_seasonality()`](https://ahaeusser.github.io/tscv/reference/test_seasonality.md)
at the specified frequency. If seasonality is not detected, the final
observation is repeated for the complete forecast horizon.

If seasonality is detected, the series is adjusted using classical
multiplicative decomposition. A naive forecast is then produced from the
seasonally adjusted series and reseasonalized using the estimated
seasonal factors.

## Usage

``` r
forecast_naive2(x, freq, n_ahead)
```

## Arguments

- x:

  A numeric vector containing the observed time series.

- freq:

  Integer value. A positive whole number specifying the seasonal
  frequency, such as \`12\` for monthly data or \`4\` for quarterly
  data.

- n_ahead:

  Integer value. A positive whole number specifying the number of future
  observations to forecast.

## Value

A numeric vector of length \`n_ahead\` containing the point forecasts.

## Examples

``` r
x <- as.numeric(AirPassengers)

forecast_naive2(
  x = x,
  freq = 12,
  n_ahead = 18
)
#>  [1] 437.4820 424.6949 484.1683 469.0476 471.6776 534.8310 589.5167 586.3231
#>  [9] 509.7019 443.0222 385.0685 432.0000 437.4820 424.6949 484.1683 469.0476
#> [17] 471.6776 534.8310
```
