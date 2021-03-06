% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DSHW.R
\name{forecast.DSHW}
\alias{forecast.DSHW}
\title{Forecast a trained DSHW model}
\usage{
\method{forecast}{DSHW}(object, new_data, specials = NULL, ...)
}
\arguments{
\item{object}{An object of class \code{DSHW}.}

\item{new_data}{Forecast horizon (n-step ahead forecast)}

\item{specials}{Specials are currently not in use.}

\item{...}{Additional arguments for forecast method.}
}
\value{
An object of class \code{fable}.
}
\description{
Forecast a trained DSHW model.
}
