% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TBATS.R
\name{train_tbats}
\alias{train_tbats}
\title{TBATS model}
\usage{
train_tbats(.data, specials, periods, ...)
}
\arguments{
\item{.data}{Input data as tsibble.}

\item{specials}{Specials as list defined in \code{specials_tbats}.}

\item{periods}{Integer vector. The periodicity of the time series (e.g. \code{periods = c(24, 168)} for hourly data).}

\item{...}{Further arguments passed to \code{forecast::tbats()}.}
}
\value{
An object of class \code{TBATS}.
}
\description{
Train a TBATS model (Trigonometric seasonality, Box-Cox transformation,
   ARMA errors, Trend and Seasonal components).
}
