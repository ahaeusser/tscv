# Expand the split_frame

The function expands the `split_frame`

## Usage

``` r
expand_split(split_frame, context)
```

## Arguments

- split_frame:

  A tibble

- context:

  A named `list` with the identifiers for `seried_id`, `value_id` and
  `index_id`.

## Value

split_frame is a tibble containing the train and test splits per time
series.
