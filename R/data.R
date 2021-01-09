#' @title Hourly day-ahead electricity spot prices
#'
#' @description Hourly tsibble with day-ahead electricity spot prices from
#'   the ENTSO-E Transparency Platform. The dataset contains time series data
#'   from 2019-01-01 00:00:00 to 2019-12-31 23:00:00 for 8 bidding zones
#'   within europe (DE, DK1, ES, FI, FR, NL, NO1, SE1).
#'
#' @docType data
#'
#' @usage data(elec_price)
#'
#' @format A time series object of class \code{tsibble} with 70.080 rows and 5 columns:
#'    \itemize{
#'       \item{\code{Time}: Date and time (index variable)}
#'       \item{\code{Series}: Time series name (key variable)}
#'       \item{\code{Unit}: Measured unit (key variable)}
#'       \item{\code{BZN}: Bidding zone (key variable)}
#'       \item{\code{Value}: Measurement variable}
#'       }
#'
#' @keywords datasets
#'
#' @source \href{https://transparency.entsoe.eu/transmission-domain/r2/dayAheadPrices/show}{ENTSO-E Transparency Platform}
#'
#' @examples
#' data(elec_price)
"elec_price"




#' @title Hourly electricity load and load forecasts
#'
#' @description Hourly tsibble with electricity loads and load forecasts
#'   from the ENTSO-E Transparency Platform. The dataset contains time series data
#'   from 2019-01-01 00:00:00 to 2019-12-31 23:00:00 for 8 bidding zones within
#'   europe (DE, DK1, ES, FI, FR, NL, NO1, SE1). The original data are on a
#'   quarter-hourly basis (15-minutes interval), but aggregated to hourly data.
#'
#' @docType data
#'
#' @usage data(elec_load)
#'
#' @format A time series object of class \code{tsibble} with 122.640 rows and 5 columns:
#'    \itemize{
#'       \item{\code{Time}: Date and time (index variable)}
#'       \item{\code{Series}: Time series name (key variable)}
#'       \item{\code{Unit}: Measured unit (key variable)}
#'       \item{\code{BZN}: Bidding zone (key variable)}
#'       \item{\code{Value}: Measurement variable}
#'       }
#'
#' @keywords datasets
#'
#' @source \href{https://transparency.entsoe.eu/load-domain/r2/totalLoadR2/show}{ENTSO-E Transparency Platform}
#'
#' @examples
#' data(elec_load)
"elec_load"




#' @title Hourly weather data
#'
#' @description Hourly \code{tsibble} with air temperature and humidity from
#'   the DWD ("Deutscher Wetterdienst"). The dataset contains time series data
#'   from 2019-01-01 00:00:00 to 2019-12-31 23:00:00 for Germany.
#'
#' @docType data
#'
#' @usage data(weather)
#'
#' @format A time series object of class \code{tsibble} with 17.520 rows and 3 columns:
#'    \itemize{
#'       \item{\code{Time}: Date and time (index variable)}
#'       \item{\code{Series}: Time series name (key variable)}
#'       \item{\code{Value}: Measurement variable}
#'       }
#'
#' @keywords datasets
#'
#' @source \href{https://cdc.dwd.de/portal/}{CDC - Climate Data Center}
#'
#' @examples
#' data(weather)
"weather"



#' @title Monthly time series data from the M4 Competition
#'
#' @description The dataset contains 30 selected time series on a monthly basis
#'    from the M4 Competition.
#'
#' @docType data
#'
#' @usage data(M4_monthly_data)
#'
#' @format A time series object of class \code{tsibble} with 7881 rows and 4 columns:
#'    \itemize{
#'       \item{\code{date_time}: Date and time (index variable)}
#'       \item{\code{series_id}: Time series ID from M4 forecasting competition (key variable)}
#'       \item{\code{category}: Category from M4 forecasting competition (key variable)}
#'       \item{\code{value}: Time series value (measurement variable)}
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
#' @description The dataset contains 30 selected time series on a quarterly basis
#'    from the M4 Competition.
#'
#' @docType data
#'
#' @usage data(M4_quarterly_data)
#'
#' @format A time series object of class \code{tsibble} with 7881 rows and 4 columns:
#'    \itemize{
#'       \item{\code{date_time}: Date and time (index variable)}
#'       \item{\code{series_id}: Time series ID from M4 forecasting competition (key variable)}
#'       \item{\code{category}: Category from M4 forecasting competition (key variable)}
#'       \item{\code{value}: Time series value (measurement variable)}
#'       }
#'
#' @keywords datasets
#'
#' @source \href{https://github.com/Mcompetitions/M4-methods/tree/master/Dataset}{M4 Competition}
#'
#' @examples
#' data(M4_quarterly_data)
"M4_quarterly_data"

