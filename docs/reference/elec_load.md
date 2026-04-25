# Hourly electricity load (actual values and forecasts)

Hourly tibble with actual electricity loads and load forecasts from the
ENTSO-E Transparency Platform. The data set contains time series data
from 2019-01-01 00:00:00 to 2019-12-31 23:00:00 for 8 bidding zones
within Europe (DE, DK1, ES, FI, FR, NL, NO1, SE1). The original data are
on a quarter-hourly basis (15-minutes interval), but aggregated to
hourly data.

## Usage

``` r
data(elec_load)
```

## Format

A time series object of class `tibble` with 140.160 rows and 5 columns:

- `time`: Date and time index

- `item`: Time series name

- `unit`: Measured unit

- `bidding_zone`: Bidding zone

- `value`: Measurement variable

## Source

[ENTSO-E Transparency
Platform](https://transparency.entsoe.eu/load-domain/r2/totalLoadR2/show)

## Examples

``` r
data(elec_load)
```
