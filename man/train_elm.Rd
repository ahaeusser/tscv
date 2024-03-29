% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ELM.R
\name{train_elm}
\alias{train_elm}
\title{Extreme Learning Machine (ELM)}
\usage{
train_elm(.data, specials, n_seed = 42, ...)
}
\arguments{
\item{.data}{Input data as tsibble.}

\item{specials}{Specials as list defined in \code{specials_elm}.}

\item{n_seed}{Integer value. The seed for the random number generator (for reproducibility).}

\item{...}{Further arguments passed to \code{nnfor::elm()}.}
}
\value{
An object of class \code{ELM}.
}
\description{
Train an Extreme Learning Machine (ELM) model.
}
