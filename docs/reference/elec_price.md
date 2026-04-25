# Hourly day-ahead electricity spot prices

Hourly tibble with day-ahead electricity spot prices from the ENTSO-E
Transparency Platform. The data set contains time series data from
2019-01-01 00:00:00 to 2020-12-31 23:00:00 for 8 bidding zones within
Europe (DE, DK1, ES, FI, FR, NL, NO1, SE1).

## Usage

``` r
data(elec_price)
```

## Format

A time series object of class `tibble` with 140.352 rows and 5 columns:

- `time`: Date and time index

- `item`: Time series name

- `unit`: Measured unit

- `bidding_zone`: Bidding zone

- `value`: Measurement variable

## Source

[ENTSO-E Transparency
Platform](https://transparency.entsoe.eu/transmission-domain/r2/dayAheadPrices/show)

## Examples

``` r
data(elec_price)
```
