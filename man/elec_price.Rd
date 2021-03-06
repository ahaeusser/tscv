% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{elec_price}
\alias{elec_price}
\title{Hourly day-ahead electricity spot prices}
\format{
A time series object of class \code{tsibble} with 70.080 rows and 5 columns:
   \itemize{
      \item{\code{Time}: Date and time (index variable)}
      \item{\code{Series}: Time series name (key variable)}
      \item{\code{Unit}: Measured unit (key variable)}
      \item{\code{BZN}: Bidding zone (key variable)}
      \item{\code{Value}: Measurement variable}
      }
}
\source{
\href{https://transparency.entsoe.eu/transmission-domain/r2/dayAheadPrices/show}{ENTSO-E Transparency Platform}
}
\usage{
data(elec_price)
}
\description{
Hourly tsibble with day-ahead electricity spot prices from
  the ENTSO-E Transparency Platform. The dataset contains time series data
  from 2019-01-01 00:00:00 to 2019-12-31 23:00:00 for 8 bidding zones
  within europe (DE, DK1, ES, FI, FR, NL, NO1, SE1).
}
\examples{
data(elec_price)
}
\keyword{datasets}
