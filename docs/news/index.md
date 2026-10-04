# Changelog

## tscv 1.0.2

- Updated unit tests for compatibility with the upcoming `fabletools`
  1.0.0 release by using
  [`is_mable()`](https://fabletools.tidyverts.org/reference/is_mable.html)
  instead of relying on the internal `mdl_df` class name.

## tscv 1.0.1

CRAN release: 2026-09-02

- Added
  [`test_seasonality()`](https://ahaeusser.github.io/tscv/reference/test_seasonality.md)
  to test for seasonality at a specified frequency using the
  autocorrelation-based procedure from the M4 Forecasting Competition.
- Added
  [`forecast_naive2()`](https://ahaeusser.github.io/tscv/reference/forecast_naive2.md)
  to generate Naive2 benchmark forecasts using conditional seasonal
  adjustment.
- Added
  [`NAIVE2()`](https://ahaeusser.github.io/tscv/reference/NAIVE2.md) and
  supporting S3 methods to integrate the Naive2 benchmark with the
  `fabletools` model interface, including forecast distributions, fitted
  values and residuals.
- Expanded the package documentation with references covering time
  series cross-validation, forecast accuracy measures, and benchmark
  forecasting methods.

## tscv 1.0.0

CRAN release: 2026-05-13

- Initial CRAN submission.
