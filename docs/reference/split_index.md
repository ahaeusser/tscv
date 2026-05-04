# Create indices for train and test splits

Create train and test indices for time series cross-validation.

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

  Integer. The total number of observations in the time series.

- n_init:

  Integer. The number of observations in the initial training window.

- n_ahead:

  Integer. The forecast horizon, i.e. the number of observations in each
  test window.

- n_skip:

  Integer. The number of observations to skip between split origins. The
  default is `0`.

- n_lag:

  Integer. The number of lagged observations to include before the test
  window. The default is `0`.

- mode:

  Character value. Either `"slide"` for a fixed-window approach or
  `"stretch"` for an expanding-window approach.

- exceed:

  Logical value. If `TRUE`, test indices may exceed the original sample
  size.

## Value

A `list` with two elements:

- `train`: a list of integer vectors with training indices.

- `test`: a list of integer vectors with test indices.

## Details

`split_index()` creates integer index vectors for rolling-origin
resampling. The function can create either fixed-window or
expanding-window splits:

- `mode = "slide"` creates a fixed training window that moves forward
  over time.

- `mode = "stretch"` creates an expanding training window that always
  starts at the first observation.

The first training window contains `n_init` observations. Each test
window contains `n_ahead` observations. The argument `n_skip` controls
how many observations are skipped between consecutive split origins. For
example, with `n_ahead = 24` and `n_skip = 23`, consecutive test windows
are non-overlapping.

If `n_lag > 0`, the test indices include lagged observations before the
forecast horizon. This is useful when lagged predictors are needed for
constructing features during testing.

If `exceed = TRUE`, additional out-of-sample test indices are allowed to
exceed the original sample size.

## See also

Other time series cross-validation:
[`make_future()`](https://ahaeusser.github.io/tscv/reference/make_future.md),
[`make_split()`](https://ahaeusser.github.io/tscv/reference/make_split.md),
[`slice_test()`](https://ahaeusser.github.io/tscv/reference/slice_test.md),
[`slice_train()`](https://ahaeusser.github.io/tscv/reference/slice_train.md)

## Examples

``` r
# Fixed-window splits
fixed_index <- split_index(
  n_total = 120,
  n_init = 48,
  n_ahead = 24,
  n_skip = 23,
  n_lag = 0,
  mode = "slide",
  exceed = FALSE
)

fixed_index
#> $train
#> $train[[1]]
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
#> [26] 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48
#> 
#> $train[[2]]
#>  [1] 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49
#> [26] 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72
#> 
#> $train[[3]]
#>  [1] 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73
#> [26] 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96
#> 
#> 
#> $test
#> $test[[1]]
#>  [1] 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72
#> 
#> $test[[2]]
#>  [1] 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96
#> 
#> $test[[3]]
#>  [1]  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115
#> [20] 116 117 118 119 120
#> 
#> 

# Expanding-window splits
expanding_index <- split_index(
  n_total = 120,
  n_init = 48,
  n_ahead = 24,
  n_skip = 23,
  n_lag = 0,
  mode = "stretch",
  exceed = FALSE
)

expanding_index
#> $train
#> $train[[1]]
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
#> [26] 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48
#> 
#> $train[[2]]
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
#> [26] 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
#> [51] 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72
#> 
#> $train[[3]]
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
#> [26] 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
#> [51] 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75
#> [76] 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96
#> 
#> 
#> $test
#> $test[[1]]
#>  [1] 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72
#> 
#> $test[[2]]
#>  [1] 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96
#> 
#> $test[[3]]
#>  [1]  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115
#> [20] 116 117 118 119 120
#> 
#> 
```
