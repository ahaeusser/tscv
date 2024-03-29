% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_errors.R
\name{make_errors}
\alias{make_errors}
\title{Calculate forecast errors and percentage errors}
\usage{
make_errors(future_frame, main_frame, context)
}
\arguments{
\item{future_frame}{A \code{tibble} containing the forecasts for the models, splits, etc.}

\item{main_frame}{A \code{tibble} containing the actual values.}

\item{context}{A named \code{list} with the identifiers for \code{seried_id}, \code{value_id} and \code{index_id}.}
}
\value{
error_frame is a \code{tibble} with forecast errors and
  percentage forecast errors.
}
\description{
\code{make_errors} calculates the forecast errors (error) and
  percentage forecast errors (pct_error).

   \itemize{
      \item{\code{series_id}: Unique identifier for the time series as specified in \code{context}}.
      \item{\code{model}: Character value. The forecasting model.}
      \item{\code{split}: Integer value. The number of the train data split.}
      \item{\code{horizon}: The forecast horizon as integer.}
      \item{\code{error}: The forecast errors as numeric value.}
      \item{\code{pct_error}: The percentage forecast errors as numeric value.}
      }
}
