# Quarterly time series data from the M4 Competition

The data set contains 30 selected time series on a quarterly basis from
the M4 Competition.

## Usage

``` r
data(M4_quarterly_data)
```

## Format

A time series object of class `tibble` with 2818 rows and 4 columns:

- `index`: Date and time index

- `series`: Time series ID from M4 forecasting competition

- `category`: Category from M4 forecasting competition

- `value`: Measurement variable

## Source

[M4
Competition](https://github.com/Mcompetitions/M4-methods/tree/master/Dataset)

## Examples

``` r
data(M4_quarterly_data)
```
