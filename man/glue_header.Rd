% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logging.R
\name{glue_header}
\alias{glue_header}
\title{Create a header from text}
\usage{
glue_header(text, n_char = 80, ft_bold = FALSE, ft_italic = FALSE)
}
\arguments{
\item{text}{Character value. The text for the header.}

\item{n_char}{Integer value. Maximum length of the header line.}

\item{ft_bold}{Logical value. If \code{TRUE}, \code{text} is printed bold.}

\item{ft_italic}{Logical value. If \code{TRUE}, \code{text} is printed italic.}
}
\value{
An object of class \code{glue}.
}
\description{
Create a header from text with \code{glue()}. White space is
 padded with "-" to a line length of \code{n_char}.
}
