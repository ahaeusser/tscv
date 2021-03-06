% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DSHW.R
\name{DSHW}
\alias{DSHW}
\title{Automatic train a DSHW model}
\usage{
DSHW(formula, ...)
}
\arguments{
\item{formula}{Model specification (see "Specials" section, currently not in use ...)}

\item{...}{Further arguments passed to \code{forecast::dshw()}.}
}
\value{
dshw_model An object of class \code{DSHW}.
}
\description{
Automatic train a Double Seasonal Holt-Winters model (DSHW). This function
   is a wrapper for \code{forecast::dshw()}.
}
