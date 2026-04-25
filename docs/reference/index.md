# Package index

## Time Series Cross-Validation

Functions and utilities for time series cross-validation

- [`make_accuracy()`](https://ahaeusser.github.io/tscv/reference/make_accuracy.md)
  : Estimate accuracy metrics to evaluate point forecast

- [`make_errors()`](https://ahaeusser.github.io/tscv/reference/make_errors.md)
  : Calculate forecast errors and percentage errors

- [`make_future()`](https://ahaeusser.github.io/tscv/reference/make_future.md)
  :

  Convert the forecasts from a `fable` to a `future_frame`

- [`make_split()`](https://ahaeusser.github.io/tscv/reference/make_split.md)
  : Create a split_frame for train and test splits per time series.

- [`make_tsibble()`](https://ahaeusser.github.io/tscv/reference/make_tsibble.md)
  : Convert tibble to tsibble

- [`slice_train()`](https://ahaeusser.github.io/tscv/reference/slice_train.md)
  : Slice the train data from the complete data

- [`slice_test()`](https://ahaeusser.github.io/tscv/reference/slice_test.md)
  : Slice the test data from the complete data

## Data visualization

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
- [`theme_tscv_dark()`](https://ahaeusser.github.io/tscv/reference/theme_tscv_dark.md)
  : Dark ggplot2 theme for tscv package
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
  Automatic train a DSHW model
- [`forecast(`*`<DSHW>`*`)`](https://ahaeusser.github.io/tscv/reference/forecast.DSHW.md)
  : Forecast a trained DSHW model
- [`fitted(`*`<DSHW>`*`)`](https://ahaeusser.github.io/tscv/reference/fitted.DSHW.md)
  : Extract fitted values from a trained DSHW model
- [`residuals(`*`<DSHW>`*`)`](https://ahaeusser.github.io/tscv/reference/residuals.DSHW.md)
  : Extract residuals from a trained DSHW model
- [`model_sum(`*`<DSHW>`*`)`](https://ahaeusser.github.io/tscv/reference/model_sum.DSHW.md)
  : Provide a succinct summary of a trained DSHW model
- [`train_dshw()`](https://ahaeusser.github.io/tscv/reference/train_dshw.md)
  : Double Seasonal Holt-Winters model

### ELM

The ELM model and its supported methods

- [`ELM()`](https://ahaeusser.github.io/tscv/reference/ELM.md) : Extreme
  Learning Machine (ELM)
- [`forecast(`*`<ELM>`*`)`](https://ahaeusser.github.io/tscv/reference/forecast.ELM.md)
  : Forecast a trained ELM model
- [`fitted(`*`<ELM>`*`)`](https://ahaeusser.github.io/tscv/reference/fitted.ELM.md)
  : Extract fitted values from a trained ELM model
- [`residuals(`*`<ELM>`*`)`](https://ahaeusser.github.io/tscv/reference/residuals.ELM.md)
  : Extract residuals from a trained ELM model
- [`model_sum(`*`<ELM>`*`)`](https://ahaeusser.github.io/tscv/reference/model_sum.ELM.md)
  : Provide a succinct summary of a trained ELM model
- [`train_elm()`](https://ahaeusser.github.io/tscv/reference/train_elm.md)
  : Extreme Learning Machine (ELM)

### MLP

The MLP model and its supported methods

- [`MLP()`](https://ahaeusser.github.io/tscv/reference/MLP.md) :
  Automatic training of MLPs
- [`forecast(`*`<MLP>`*`)`](https://ahaeusser.github.io/tscv/reference/forecast.MLP.md)
  : Forecast a trained MLP model
- [`fitted(`*`<MLP>`*`)`](https://ahaeusser.github.io/tscv/reference/fitted.MLP.md)
  : Extract fitted values from a trained MLP model
- [`residuals(`*`<MLP>`*`)`](https://ahaeusser.github.io/tscv/reference/residuals.MLP.md)
  : Extract residuals from a trained MLP model
- [`model_sum(`*`<MLP>`*`)`](https://ahaeusser.github.io/tscv/reference/model_sum.MLP.md)
  : Provide a succinct summary of a trained MLP model
- [`train_mlp()`](https://ahaeusser.github.io/tscv/reference/train_mlp.md)
  : Multilayer Perceptron (MLP)

### SMEAN

The SMEAN model and its supported methods

- [`SMEAN()`](https://ahaeusser.github.io/tscv/reference/SMEAN.md) :
  Seasonal mean model
- [`forecast(`*`<SMEAN>`*`)`](https://ahaeusser.github.io/tscv/reference/forecast.SMEAN.md)
  : Forecast a trained seasonal mean model
- [`fitted(`*`<SMEAN>`*`)`](https://ahaeusser.github.io/tscv/reference/fitted.SMEAN.md)
  : Extract fitted values from a trained seasonal mean model
- [`residuals(`*`<SMEAN>`*`)`](https://ahaeusser.github.io/tscv/reference/residuals.SMEAN.md)
  : Extract residuals from a trained seasonal mean model
- [`model_sum(`*`<SMEAN>`*`)`](https://ahaeusser.github.io/tscv/reference/model_sum.SMEAN.md)
  : Provide a succinct summary of a trained seasonal mean model
- [`train_smean()`](https://ahaeusser.github.io/tscv/reference/train_smean.md)
  : Seasonal mean model

### SMEDIAN

The SMEDIAN model and its supported methods

- [`SMEDIAN()`](https://ahaeusser.github.io/tscv/reference/SMEDIAN.md) :
  Seasonal median model
- [`forecast(`*`<SMEDIAN>`*`)`](https://ahaeusser.github.io/tscv/reference/forecast.SMEDIAN.md)
  : Forecast a trained seasonal median model
- [`fitted(`*`<SMEDIAN>`*`)`](https://ahaeusser.github.io/tscv/reference/fitted.SMEDIAN.md)
  : Extract fitted values from a trained seasonal median model
- [`residuals(`*`<SMEDIAN>`*`)`](https://ahaeusser.github.io/tscv/reference/residuals.SMEDIAN.md)
  : Extract residuals from a trained seasonal median model
- [`model_sum(`*`<SMEDIAN>`*`)`](https://ahaeusser.github.io/tscv/reference/model_sum.SMEDIAN.md)
  : Provide a succinct summary of a trained seasonal median model
- [`train_smedian()`](https://ahaeusser.github.io/tscv/reference/train_smedian.md)
  : Seasonal median model

### MEDIAN

The MEDIAN model and its supported methods

- [`MEDIAN()`](https://ahaeusser.github.io/tscv/reference/MEDIAN.md) :
  Median model
- [`forecast(`*`<MEDIAN>`*`)`](https://ahaeusser.github.io/tscv/reference/forecast.MEDIAN.md)
  : Forecast a trained median model
- [`fitted(`*`<MEDIAN>`*`)`](https://ahaeusser.github.io/tscv/reference/fitted.MEDIAN.md)
  : Extract fitted values from a trained median model
- [`residuals(`*`<MEDIAN>`*`)`](https://ahaeusser.github.io/tscv/reference/residuals.MEDIAN.md)
  : Extract residuals from a trained median model
- [`model_sum(`*`<MEDIAN>`*`)`](https://ahaeusser.github.io/tscv/reference/model_sum.MEDIAN.md)
  : Provide a succinct summary of a trained median model
- [`train_median()`](https://ahaeusser.github.io/tscv/reference/train_median.md)
  : Median model

### SNAIVE2

The SNAIVE2 model and its supported methods

- [`SNAIVE2()`](https://ahaeusser.github.io/tscv/reference/SNAIVE2.md) :
  Seasonal naive model
- [`forecast(`*`<SNAIVE2>`*`)`](https://ahaeusser.github.io/tscv/reference/forecast.SNAIVE2.md)
  : Forecast a trained seasonal naive model
- [`fitted(`*`<SNAIVE2>`*`)`](https://ahaeusser.github.io/tscv/reference/fitted.SNAIVE2.md)
  : Extract fitted values from a trained seasonal naive model
- [`residuals(`*`<SNAIVE2>`*`)`](https://ahaeusser.github.io/tscv/reference/residuals.SNAIVE2.md)
  : Extract residuals from a trained seasonal naive model
- [`model_sum(`*`<SNAIVE2>`*`)`](https://ahaeusser.github.io/tscv/reference/model_sum.SNAIVE2.md)
  : Provide a succinct summary of a trained seasonal naive model
- [`train_snaive2()`](https://ahaeusser.github.io/tscv/reference/train_snaive2.md)
  : Seasonal naive model

### TBATS

The TBATS model and its supported methods

- [`TBATS()`](https://ahaeusser.github.io/tscv/reference/TBATS.md) :
  Automatic train a TBATS model
- [`forecast(`*`<TBATS>`*`)`](https://ahaeusser.github.io/tscv/reference/forecast.TBATS.md)
  : Forecast a trained TBATS model
- [`fitted(`*`<TBATS>`*`)`](https://ahaeusser.github.io/tscv/reference/fitted.TBATS.md)
  : Extract fitted values from a trained TBATS model
- [`residuals(`*`<TBATS>`*`)`](https://ahaeusser.github.io/tscv/reference/residuals.TBATS.md)
  : Extract residuals from a trained TBATS model
- [`model_sum(`*`<TBATS>`*`)`](https://ahaeusser.github.io/tscv/reference/model_sum.TBATS.md)
  : Provide a succinct summary of a trained TBATS model
- [`train_tbats()`](https://ahaeusser.github.io/tscv/reference/train_tbats.md)
  : TBATS model

### EXPERT

The EXPERT model and its supported methods

- [`EXPERT()`](https://ahaeusser.github.io/tscv/reference/EXPERT.md) :
  Automatic train a EXPERT model
- [`forecast(`*`<EXPERT>`*`)`](https://ahaeusser.github.io/tscv/reference/forecast.EXPERT.md)
  : Forecast a trained EXPERT model
- [`fitted(`*`<EXPERT>`*`)`](https://ahaeusser.github.io/tscv/reference/fitted.EXPERT.md)
  : Extract fitted values from a trained EXPERT model
- [`residuals(`*`<EXPERT>`*`)`](https://ahaeusser.github.io/tscv/reference/residuals.EXPERT.md)
  : Extract residuals from a trained EXPERT model
- [`model_sum(`*`<EXPERT>`*`)`](https://ahaeusser.github.io/tscv/reference/model_sum.EXPERT.md)
  : Provide a succinct summary of a trained EXPERT model
- [`train_expert()`](https://ahaeusser.github.io/tscv/reference/train_expert.md)
  : EXPERT model

## Miscellaneous

Functions and utilities for data preparation, etc.

- [`estimate_mode()`](https://ahaeusser.github.io/tscv/reference/estimate_mode.md)
  : Estimate mode of a distribution based on Kernel Density Estimation
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
- [`lst_to_env()`](https://ahaeusser.github.io/tscv/reference/lst_to_env.md)
  : Assign objects within a list to an environment
- [`check_data()`](https://ahaeusser.github.io/tscv/reference/check_data.md)
  : Check, convert and shape the input data
- [`summarise_data()`](https://ahaeusser.github.io/tscv/reference/summarise_data.md)
  : Summary statistics for time series data
- [`summarise_stats()`](https://ahaeusser.github.io/tscv/reference/summarise_stats.md)
  : Summary statistics for time series data
- [`summarise_split()`](https://ahaeusser.github.io/tscv/reference/summarise_split.md)
  : Summary table of the splitting into training and testing
- [`initialize_split()`](https://ahaeusser.github.io/tscv/reference/initialize_split.md)
  : Initialize a plan for train-test split
- [`split_index()`](https://ahaeusser.github.io/tscv/reference/split_index.md)
  : Create indices for train and test splits.
- [`expand_split()`](https://ahaeusser.github.io/tscv/reference/expand_split.md)
  : Expand the split_frame
- [`file_name()`](https://ahaeusser.github.io/tscv/reference/file_name.md)
  : Create a name for a folder or file
- [`number_string()`](https://ahaeusser.github.io/tscv/reference/number_string.md)
  : Helper function to create numbered strings.
- [`` `%out%` ``](https://ahaeusser.github.io/tscv/reference/grapes-out-grapes.md)
  : Negated value matching
- [`glue_header()`](https://ahaeusser.github.io/tscv/reference/glue_header.md)
  : Create a header from text
- [`log_platform()`](https://ahaeusser.github.io/tscv/reference/log_platform.md)
  : Create platform info
- [`log_time()`](https://ahaeusser.github.io/tscv/reference/log_time.md)
  : Create string with elapsed time.

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
