% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_split.R
\name{make_split}
\alias{make_split}
\title{Create a split_frame for train and test splits per time series.}
\usage{
make_split(
  main_frame,
  context,
  type,
  value,
  n_ahead,
  n_skip = 0,
  n_lag = 0,
  mode = "slide",
  exceed = TRUE
)
}
\arguments{
\item{main_frame}{A \code{tibble} containing the time series data.}

\item{context}{A named \code{list} with the identifiers for \code{seried_id}, \code{value_id} and \code{index_id}.}

\item{type}{The type for the initial split. Possible values are \code{"first"}, \code{"last"}, \code{"prob"}.}

\item{value}{Numeric value specifying the split.}

\item{n_ahead}{The forecast horizon (n-steps-ahead, must be positive).}

\item{n_skip}{The number of periods to skip between windows (must be zero or positive integer).}

\item{n_lag}{A value to include a lag between the training and testing set. This is useful if lagged predictors will be used during training and testing.}

\item{mode}{Character value. Define the setup of the training window for time series cross validation. \code{stretch} is equivalent to an expanding window approach and \code{slide} is a fixed window approach.}

\item{exceed}{Logical value. If \code{TRUE}, out-of-sample splits exceeding the sample size are created.}
}
\value{
A \code{list} containing the indices for train and test as integer vectors.
}
\description{
The function creates the split indices for train and test samples
  (i.e. partitioning into time slices) for time series cross-validation. The
  user can choose between \code{stretch} and \code{slide}. The first is an
  expanding window approach, while the latter is a fixed window approach.
  The user can define the window sizes for training and testing via
  \code{n_init} and \code{n_ahead}, as well as the step size for increments
  via \code{n_step}.
}
