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
#' @format A time series object of class \code{tsibble} with 43.800 rows and 3 columns:
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




#' @title Daily Sales Data from the M5-Forecasting Competition
#'
#' @description Daily "tsibble" with sales data from the M5-Forecasting
#'    Competition. The dataset contains time series data
#'    from 2011-01-29 to 2016-04-24. The sales data are aggregated for
#'    category (cat_id), stores (store_id) and states (state_id).
#'
#' @docType data
#'
#' @usage data(m5_data)
#'
#' @format A time series object of class \code{tsibble} with 172.170 rows and 10 columns:
#'    \itemize{
#'       \item{variable: Unique identifier for category, store and state (key variable)}
#'       \item{cat_id: Product category (Hobbies, Foods, Household)}
#'       \item{store_id: Store number}
#'       \item{state_id: State (CA, TX, WI)}
#'       \item{date: Date (index variable)}
#'       \item{year: Year}
#'       \item{month: Month}
#'       \item{day: Day}
#'       \item{wday: Weekday}
#'       \item{value: Sales data (measurement variable)}
#'       }
#'
#' @keywords datasets
#'
#' @source \href{https://www.kaggle.com/c/m5-forecasting-accuracy/data}{M5-Forecasting - Accuracy}
#'
#' @examples
#' data(m5_data)
"m5_data"
