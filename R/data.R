#' @title Hourly day-ahead electricity spot prices
#'
#' @description Hourly \code{tsibble} with day-ahead electricity spot prices from
#'    the ENTSO-E Transparency Platform. The dataset contains time series data
#'    from 2019-01-01 00:00:00 to 2019-12-31 23:00:00 for four bidding zones
#'    within europe (DE, FR, SE1, NO1).
#'
#' @docType data
#'
#' @usage data(elec_price)
#'
#' @format A time series object of class \code{tsibble} with 35.040 rows and 4 columns:
#'    \itemize{
#'       \item{\code{Time}: Date and time (index variable)}
#'       \item{\code{Series}: Time series name (key variable)}
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
#' @description Hourly \code{tsibble} with electricity loads from
#'    the ENTSO-E Transparency Platform. The dataset contains time series data
#'    from 2019-01-01 00:00:00 to 2019-12-31 23:00:00 for four bidding zones
#'    within europe (DE, FR, SE1, NO1).
#'
#' @docType data
#'
#' @usage data(elec_load)
#'
#' @format A time series object of class \code{tsibble} with 35.040 rows and 4 columns:
#'    \itemize{
#'       \item{\code{Time}: Date and time (index variable)}
#'       \item{\code{Series}: Time series name (key variable)}
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
