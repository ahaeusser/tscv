# Convert tibble to tsibble

`make_tsibble` is a helper function to convert a `tibble` to a
`tsibble`.

## Usage

``` r
make_tsibble(main_frame, context)
```

## Arguments

- main_frame:

  A `tibble` containing the time series data.

- context:

  A named `list` with the identifiers for `seried_id`, `value_id` and
  `index_id`.

## Value

main_frame Same structure as before, just stored as `tsibble`.
