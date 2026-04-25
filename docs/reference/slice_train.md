# Slice the train data from the complete data

`slice_train` creates the train data from the complete data set
`main_frame` according to `split_frame`. Same columns as `main_frame`,
but the number of the split is added as integer.

## Usage

``` r
slice_train(main_frame, split_frame, context)
```

## Arguments

- main_frame:

  A `tibble` containing the time series data.

- split_frame:

  A `tibble` containing the splits into train and test. The result of a
  call to
  [`make_split()`](https://ahaeusser.github.io/tscv/reference/make_split.md).

- context:

  A named `list` with the identifiers for `seried_id`, `value_id` and
  `index_id`.

## Value

train_frame is a `tibble` containing the train data.
