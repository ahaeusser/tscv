# Initialize a plan for train-test split

The function creates the initial split into training and testing data.

## Usage

``` r
initialize_split(
  main_frame,
  context,
  type = c("first", "last", "prob"),
  value = NULL
)
```

## Arguments

- main_frame:

  A `tibble` containing the time series data.

- context:

  A named `list` with the identifiers for `seried_id`, `value_id` and
  `index_id`.

- type:

  The type for the initial split. Possible values are `"first"`,
  `"last"`, `"prob"`.

- value:

  Numeric value specifying the split.

## Value

A `tibble`
