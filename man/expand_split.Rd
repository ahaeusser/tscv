% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_split.R
\name{expand_split}
\alias{expand_split}
\title{Expand the split_frame}
\usage{
expand_split(split_frame, context)
}
\arguments{
\item{split_frame}{A tibble}

\item{context}{A named \code{list} with the identifiers for \code{seried_id}, \code{value_id} and \code{index_id}.}
}
\value{
split_frame is a tibble containing the train and test splits per time series.
}
\description{
The function expands the \code{split_frame}
}
