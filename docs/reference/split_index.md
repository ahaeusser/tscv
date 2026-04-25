# Create indices for train and test splits.

The function creates the split indices for train and test samples (i.e.
partitioning into time slices) for time series cross-validation. The
user can choose between `stretch` and `slide`. The first is an expanding
window approach, while the latter is a fixed window approach. The user
can define the window sizes for training and testing via `n_init` and
`n_ahead`, as well as the step size for increments via `n_step`.

## Usage

``` r
split_index(
  n_total,
  n_init,
  n_ahead,
  n_skip = 0,
  n_lag = 0,
  mode = "slide",
  exceed = FALSE
)
```

## Arguments

- n_total:

  The total number of observations of the time series.

- n_init:

  The number of periods for the initial training window (must be
  positive).

- n_ahead:

  The forecast horizon (n-steps-ahead, must be positive).

- n_skip:

  The number of periods to skip between windows (must be zero or
  positive integer).

- n_lag:

  A value to include a lag between the training and testing set. This is
  useful if lagged predictors will be used during training and testing.

- mode:

  Character value. Define the setup of the training window for time
  series cross validation. `stretch` is equivalent to an expanding
  window approach and `slide` is a fixed window approach.

- exceed:

  Logical value. If `TRUE`, out-of-sample splits exceeding the sample
  size are created.

## Value

A `list` containing the indices for train and test as integer vectors.
