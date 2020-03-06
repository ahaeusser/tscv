#' @title Hourly Day-ahead Electricity Spot Prices
#'
#' @description Hourly "tsibble" with day-ahead electricity spot prices from
#'    the ENTSO-E Transparency Platform. The dataset contains time series data
#'    from 2019-01-01 00:00:00 to 2019-12-31 23:00:00 for five bidding zones
#'    within europe (DE, FR, NL, SE1, NO1).
#'
#' @docType data
#'
#' @usage data(elec_price)
#'
#' @format A time series object of class \code{tsibble} with 43.800 rows and 3 variables:
#'    \itemize{
#'       \item{date_time: Date and time (index variable)}
#'       \item{variable: Variable name (key variable)}
#'       \item{value: Day-ahead spot price in Euro (measurement variable)}
#'       }
#'
#' @keywords datasets
#'
#' @source \href{https://transparency.entsoe.eu/transmission-domain/r2/dayAheadPrices/show}{ENTSO-E Transparency Platform}
#'
#' @examples
#' data(elec_price)
"elec_price"
