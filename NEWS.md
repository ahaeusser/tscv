# tscv 1.0.1

* Added `test_seasonality()` to test for seasonality at a specified frequency using the autocorrelation-based procedure from the M4 Forecasting Competition.
* Added `forecast_naive2()` to generate Naive2 benchmark forecasts using conditional seasonal adjustment.
* Added `NAIVE2()` and supporting S3 methods to integrate the Naive2 benchmark with the `fabletools` model interface, including forecast distributions, fitted values and residuals.
* Expanded the package documentation with references covering time series cross-validation, forecast accuracy measures, and benchmark forecasting methods.

# tscv 1.0.0

* Initial CRAN submission.
