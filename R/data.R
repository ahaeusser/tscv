#' @title Hourly day-ahead electricity spot prices
#'
#' @description Hourly \code{tsibble} with day-ahead electricity spot prices from
#'    the ENTSO-E Transparency Platform. The dataset contains time series data
#'    from 2019-01-01 00:00:00 to 2019-12-31 23:00:00 for 8 bidding zones
#'    within europe (DE, DK1, ES, FI, FR, NL, NO1, SE1).
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




#' @title Hourly electricity load
#'
#' @description Hourly \code{tsibble} with electricity loads and load forecasts
#'    from the ENTSO-E Transparency Platform. The dataset contains time series data
#'    from 2019-01-01 00:00:00 to 2019-12-31 23:00:00 for 8 bidding zones within
#'    europe (DE, DK1, ES, FI, FR, NL, NO1, SE1). The data are aggregated in time
#'    from a 15-minutes interval to an hourly basis.
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
#'    the DWD ("Deutscher Wetterdienst"). The dataset contains time series data
#'    from 2019-01-01 00:00:00 to 2019-12-31 23:00:00 for Germany.
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
