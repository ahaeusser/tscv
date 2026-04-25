# Summary table of the splitting into training and testing

This functions provides a summary table of the splitting into the
training and testing data giving the start and end date for each
train-test sample and the corresponding index as integer.. This means,
the function is applied to the result of a call to `split_data(...)`.

- `slice`: Time slice

- `dates_train`: Date (or date-time) range of the training data

- `dates_test`: Date (or date-time) range of the testing data

- `index_train`: Index range of the training data

- `index_test`: Index range of the testing data

## Usage

``` r
summarise_split(data)
```

## Arguments

- data:

  A valid tsibble in long format with one measurement variable and
  multiple keys. One of the keys must be `slice`.

## Value

split_tbl A tibble containing the summarized values.
