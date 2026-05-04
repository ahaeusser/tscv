# Package index

## Time Series Cross-Validation

Functions and utilities for time series cross-validation

- [`make_split()`](https://ahaeusser.github.io/tscv/reference/make_split.md)
  : Create train-test splits for time series cross-validation
- [`slice_train()`](https://ahaeusser.github.io/tscv/reference/slice_train.md)
  : Slice training data from a split frame
- [`slice_test()`](https://ahaeusser.github.io/tscv/reference/slice_test.md)
  : Slice test data from a split frame
- [`split_index()`](https://ahaeusser.github.io/tscv/reference/split_index.md)
  : Create indices for train and test splits
- [`make_future()`](https://ahaeusser.github.io/tscv/reference/make_future.md)
  : Convert forecasts to a future frame
- [`make_tsibble()`](https://ahaeusser.github.io/tscv/reference/make_tsibble.md)
  : Convert tibble to tsibble

## Forecast Accuracy

Functions and utilities for forecast accuracy

- [`make_accuracy()`](https://ahaeusser.github.io/tscv/reference/make_accuracy.md)
  : Estimate point forecast accuracy
- [`make_errors()`](https://ahaeusser.github.io/tscv/reference/make_errors.md)
  : Calculate forecast errors and percentage errors
- [`me_vec()`](https://ahaeusser.github.io/tscv/reference/me_vec.md) :
  Calculate the mean error
- [`mae_vec()`](https://ahaeusser.github.io/tscv/reference/mae_vec.md) :
  Calculate the mean absolute error
- [`mse_vec()`](https://ahaeusser.github.io/tscv/reference/mse_vec.md) :
  Calculate the mean squared error
- [`rmse_vec()`](https://ahaeusser.github.io/tscv/reference/rmse_vec.md)
  : Calculate the root mean squared error
- [`mpe_vec()`](https://ahaeusser.github.io/tscv/reference/mpe_vec.md) :
  Calculate the mean percentage error
- [`mape_vec()`](https://ahaeusser.github.io/tscv/reference/mape_vec.md)
  : Calculate the mean absolute percentage error
- [`smape_vec()`](https://ahaeusser.github.io/tscv/reference/smape_vec.md)
  : Calculate the symmetric mean absolute percentage error

## Data Visualization

Functions and utilities for data visualization

- [`plot_bar()`](https://ahaeusser.github.io/tscv/reference/plot_bar.md)
  : Plot data as bar chart
- [`plot_density()`](https://ahaeusser.github.io/tscv/reference/plot_density.md)
  : Plot the density via Kernel Density Estimator
- [`plot_histogram()`](https://ahaeusser.github.io/tscv/reference/plot_histogram.md)
  : Plot data as histogram
- [`plot_line()`](https://ahaeusser.github.io/tscv/reference/plot_line.md)
  : Plot data as line chart
- [`plot_point()`](https://ahaeusser.github.io/tscv/reference/plot_point.md)
  : Plot data as scatterplot
- [`plot_qq()`](https://ahaeusser.github.io/tscv/reference/plot_qq.md) :
  Quantile-Quantile plot
- [`theme_tscv()`](https://ahaeusser.github.io/tscv/reference/theme_tscv.md)
  : Custom ggplot2 theme for tscv package
- [`scale_color_tscv()`](https://ahaeusser.github.io/tscv/reference/scale_color_tscv.md)
  : Color scale constructor for tscv colors.
- [`scale_fill_tscv()`](https://ahaeusser.github.io/tscv/reference/scale_fill_tscv.md)
  : Fill scale constructor for tscv colors.
- [`tscv_cols()`](https://ahaeusser.github.io/tscv/reference/tscv_cols.md)
  : Function to extract tscv colors as hex codes.
- [`tscv_pal()`](https://ahaeusser.github.io/tscv/reference/tscv_pal.md)
  : Return function to interpolate a tscv color palette.

## Forecasting

### DSHW

The DSHW model and its supported methods

- [`DSHW()`](https://ahaeusser.github.io/tscv/reference/DSHW.md) :
  Double Seasonal Holt-Winters model
- [`forecast(`*`<DSHW>`*`)`](https://ahaeusser.github.io/tscv/reference/forecast.DSHW.md)
  : Forecast a DSHW model
- [`fitted(`*`<DSHW>`*`)`](https://ahaeusser.github.io/tscv/reference/fitted.DSHW.md)
  : Extract fitted values from a DSHW model
- [`residuals(`*`<DSHW>`*`)`](https://ahaeusser.github.io/tscv/reference/residuals.DSHW.md)
  : Extract residuals from a DSHW model
- [`model_sum(`*`<DSHW>`*`)`](https://ahaeusser.github.io/tscv/reference/model_sum.DSHW.md)
  : Summarize a DSHW model

### SMEAN

The SMEAN model and its supported methods

- [`SMEAN()`](https://ahaeusser.github.io/tscv/reference/SMEAN.md) :
  Seasonal mean model
- [`forecast(`*`<SMEAN>`*`)`](https://ahaeusser.github.io/tscv/reference/forecast.SMEAN.md)
  : Forecast a seasonal mean model
- [`fitted(`*`<SMEAN>`*`)`](https://ahaeusser.github.io/tscv/reference/fitted.SMEAN.md)
  : Extract fitted values from a seasonal mean model
- [`residuals(`*`<SMEAN>`*`)`](https://ahaeusser.github.io/tscv/reference/residuals.SMEAN.md)
  : Extract residuals from a seasonal mean model
- [`model_sum(`*`<SMEAN>`*`)`](https://ahaeusser.github.io/tscv/reference/model_sum.SMEAN.md)
  : Summarize a seasonal mean model

### SMEDIAN

The SMEDIAN model and its supported methods

- [`SMEDIAN()`](https://ahaeusser.github.io/tscv/reference/SMEDIAN.md) :
  Seasonal median model
- [`forecast(`*`<SMEDIAN>`*`)`](https://ahaeusser.github.io/tscv/reference/forecast.SMEDIAN.md)
  : Forecast a seasonal median model
- [`fitted(`*`<SMEDIAN>`*`)`](https://ahaeusser.github.io/tscv/reference/fitted.SMEDIAN.md)
  : Extract fitted values from a seasonal median model
- [`residuals(`*`<SMEDIAN>`*`)`](https://ahaeusser.github.io/tscv/reference/residuals.SMEDIAN.md)
  : Extract residuals from a seasonal median model
- [`model_sum(`*`<SMEDIAN>`*`)`](https://ahaeusser.github.io/tscv/reference/model_sum.SMEDIAN.md)
  : Summarize a seasonal median model

### MEDIAN

The MEDIAN model and its supported methods

- [`MEDIAN()`](https://ahaeusser.github.io/tscv/reference/MEDIAN.md) :
  Median model
- [`forecast(`*`<MEDIAN>`*`)`](https://ahaeusser.github.io/tscv/reference/forecast.MEDIAN.md)
  : Forecast a median model
- [`fitted(`*`<MEDIAN>`*`)`](https://ahaeusser.github.io/tscv/reference/fitted.MEDIAN.md)
  : Extract fitted values from a median model
- [`residuals(`*`<MEDIAN>`*`)`](https://ahaeusser.github.io/tscv/reference/residuals.MEDIAN.md)
  : Extract residuals from a median model
- [`model_sum(`*`<MEDIAN>`*`)`](https://ahaeusser.github.io/tscv/reference/model_sum.MEDIAN.md)
  : Summarize a median model

### SNAIVE2

The SNAIVE2 model and its supported methods

- [`SNAIVE2()`](https://ahaeusser.github.io/tscv/reference/SNAIVE2.md) :
  Seasonal naive model with weekday-specific lags
- [`forecast(`*`<SNAIVE2>`*`)`](https://ahaeusser.github.io/tscv/reference/forecast.SNAIVE2.md)
  : Forecast a SNAIVE2 model
- [`fitted(`*`<SNAIVE2>`*`)`](https://ahaeusser.github.io/tscv/reference/fitted.SNAIVE2.md)
  : Extract fitted values from a SNAIVE2 model
- [`residuals(`*`<SNAIVE2>`*`)`](https://ahaeusser.github.io/tscv/reference/residuals.SNAIVE2.md)
  : Extract residuals from a SNAIVE2 model
- [`model_sum(`*`<SNAIVE2>`*`)`](https://ahaeusser.github.io/tscv/reference/model_sum.SNAIVE2.md)
  : Summarize a SNAIVE2 model

### TBATS

The TBATS model and its supported methods

- [`TBATS()`](https://ahaeusser.github.io/tscv/reference/TBATS.md) :
  TBATS model
- [`forecast(`*`<TBATS>`*`)`](https://ahaeusser.github.io/tscv/reference/forecast.TBATS.md)
  : Forecast a TBATS model
- [`fitted(`*`<TBATS>`*`)`](https://ahaeusser.github.io/tscv/reference/fitted.TBATS.md)
  : Extract fitted values from a TBATS model
- [`residuals(`*`<TBATS>`*`)`](https://ahaeusser.github.io/tscv/reference/residuals.TBATS.md)
  : Extract residuals from a TBATS model
- [`model_sum(`*`<TBATS>`*`)`](https://ahaeusser.github.io/tscv/reference/model_sum.TBATS.md)
  : Summarize a TBATS model

## Miscellaneous

Functions and utilities for data preparation, etc.

- [`estimate_mode()`](https://ahaeusser.github.io/tscv/reference/estimate_mode.md)
  : Estimate the mode of a distribution
- [`estimate_kurtosis()`](https://ahaeusser.github.io/tscv/reference/estimate_kurtosis.md)
  : Estimate kurtosis
- [`estimate_skewness()`](https://ahaeusser.github.io/tscv/reference/estimate_skewness.md)
  : Estimate skewness
- [`acf_vec()`](https://ahaeusser.github.io/tscv/reference/acf_vec.md) :
  Estimate the sample autocorrelation of a numeric vector
- [`estimate_acf()`](https://ahaeusser.github.io/tscv/reference/estimate_acf.md)
  : Estimate the sample autocorrelation
- [`pacf_vec()`](https://ahaeusser.github.io/tscv/reference/pacf_vec.md)
  : Estimate the sample partial autocorrelation of a numeric vector
- [`estimate_pacf()`](https://ahaeusser.github.io/tscv/reference/estimate_pacf.md)
  : Estimate the sample partial autocorrelation
- [`interpolate_missing()`](https://ahaeusser.github.io/tscv/reference/interpolate_missing.md)
  : Interpolate missing values
- [`smooth_outlier()`](https://ahaeusser.github.io/tscv/reference/smooth_outlier.md)
  : Identify and replace outliers
- [`check_data()`](https://ahaeusser.github.io/tscv/reference/check_data.md)
  : Check and prepare tsibble data
- [`summarise_data()`](https://ahaeusser.github.io/tscv/reference/summarise_data.md)
  : Summary statistics for time series data
- [`summarise_stats()`](https://ahaeusser.github.io/tscv/reference/summarise_stats.md)
  : Summary statistics for time series data
- [`summarise_split()`](https://ahaeusser.github.io/tscv/reference/summarise_split.md)
  : Summary table of the splitting into training and testing

## Data sets

Example data sets

- [`elec_load`](https://ahaeusser.github.io/tscv/reference/elec_load.md)
  : Hourly electricity load (actual values and forecasts)
- [`elec_price`](https://ahaeusser.github.io/tscv/reference/elec_price.md)
  : Hourly day-ahead electricity spot prices
- [`M4_monthly_data`](https://ahaeusser.github.io/tscv/reference/M4_monthly_data.md)
  : Monthly time series data from the M4 Competition
- [`M4_quarterly_data`](https://ahaeusser.github.io/tscv/reference/M4_quarterly_data.md)
  : Quarterly time series data from the M4 Competition
