
#' @title Hourly day-ahead electricity spot prices
#'
#' @description Hourly tibble with day-ahead electricity spot prices from
#'   the ENTSO-E Transparency Platform. The data set contains time series data
#'   from 2019-01-01 00:00:00 to 2020-12-31 23:00:00 for 8 bidding zones
#'   within Europe (DE, DK1, ES, FI, FR, NL, NO1, SE1).
#'
#' @docType data
#'
#' @usage data(elec_price)
#'
#' @format A time series object of class \code{tibble} with 140.352 rows and 5 columns:
#'    \itemize{
#'       \item{\code{time}: Date and time index}
#'       \item{\code{item}: Time series name}
#'       \item{\code{unit}: Measured unit}
#'       \item{\code{bidding_zone}: Bidding zone}
#'       \item{\code{value}: Measurement variable}
#'       }
#'
#' @keywords datasets
#'
#' @source \href{https://transparency.entsoe.eu/transmission-domain/r2/dayAheadPrices/show}{ENTSO-E Transparency Platform}
#'
#' @examples
#' data(elec_price)
"elec_price"


#' @title Hourly electricity load (actual values and forecasts)
#'
#' @description Hourly tibble with actual electricity loads and load forecasts
#'   from the ENTSO-E Transparency Platform. The data set contains time series data
#'   from 2019-01-01 00:00:00 to 2019-12-31 23:00:00 for 8 bidding zones within
#'   Europe (DE, DK1, ES, FI, FR, NL, NO1, SE1). The original data are on a
#'   quarter-hourly basis (15-minutes interval), but aggregated to hourly data.
#'
#' @docType data
#'
#' @usage data(elec_load)
#'
#' @format A time series object of class \code{tibble} with 140.160 rows and 5 columns:
#'    \itemize{
#'       \item{\code{time}: Date and time index}
#'       \item{\code{item}: Time series name}
#'       \item{\code{unit}: Measured unit}
#'       \item{\code{bidding_zone}: Bidding zone}
#'       \item{\code{value}: Measurement variable}
#'       }
#'
#' @keywords datasets
#'
#' @source \href{https://transparency.entsoe.eu/load-domain/r2/totalLoadR2/show}{ENTSO-E Transparency Platform}
#'
#' @examples
#' data(elec_load)
"elec_load"


#' @title Monthly time series data from the M4 Competition
#'
#' @description The data set contains 30 selected time series on a monthly basis
#'    from the M4 Competition.
#'
#' @docType data
#'
#' @usage data(M4_monthly_data)
#'
#' @format A time series object of class \code{tibble} with 7881 rows and 4 columns:
#'    \itemize{
#'       \item{\code{index}: Date and time index}
#'       \item{\code{series}: Time series ID from M4 forecasting competition}
#'       \item{\code{category}: Category from M4 forecasting competition}
#'       \item{\code{value}: Measurement variable}
#'       }
#'
#' @keywords datasets
#'
#' @source \href{https://github.com/Mcompetitions/M4-methods/tree/master/Dataset}{M4 Competition}
#'
#' @examples
#' data(M4_monthly_data)
"M4_monthly_data"


#' @title Quarterly time series data from the M4 Competition
#'
#' @description The data set contains 30 selected time series on a quarterly basis
#'    from the M4 Competition.
#'
#' @docType data
#'
#' @usage data(M4_quarterly_data)
#'
#' @format A time series object of class \code{tibble} with 2818 rows and 4 columns:
#'    \itemize{
#'       \item{\code{index}: Date and time index}
#'       \item{\code{series}: Time series ID from M4 forecasting competition}
#'       \item{\code{category}: Category from M4 forecasting competition}
#'       \item{\code{value}: Measurement variable}
#'       }
#'
#' @keywords datasets
#'
#' @source \href{https://github.com/Mcompetitions/M4-methods/tree/master/Dataset}{M4 Competition}
#'
#' @examples
#' data(M4_quarterly_data)
"M4_quarterly_data"
