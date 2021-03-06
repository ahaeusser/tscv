% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_line.R
\name{plot_line}
\alias{plot_line}
\title{Plot data as line chart}
\usage{
plot_line(
  data,
  x,
  y,
  facet_var = NULL,
  facet_scale = "free",
  facet_nrow = NULL,
  facet_ncol = NULL,
  color = NULL,
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  caption = NULL,
  line_size = 0.75,
  line_type = "solid",
  line_color = "grey35",
  line_alpha = 1,
  theme_set = theme_tscv(),
  theme_config = list(),
  ...
)
}
\arguments{
\item{data}{A \code{data.frame}, \code{tibble} or \code{tsibble} in long format.}

\item{x}{Unquoted column within \code{.data}.}

\item{y}{Unquoted column within \code{.data} containing numeric values.}

\item{facet_var}{Unquoted column within \code{.data} (facet).}

\item{facet_scale}{Character value defining axis scaling (\code{facet_var = "free"} or \code{facet_var = "fixed"}).}

\item{facet_nrow}{Integer value. The number of rows.}

\item{facet_ncol}{Integer value. The number of columns.}

\item{color}{Unquoted column within \code{.data} (color).}

\item{title}{Title of the plot.}

\item{subtitle}{Subtitle of the plot.}

\item{xlab}{Label for the x-axis.}

\item{ylab}{Label for the y-axis.}

\item{caption}{Caption of the plot.}

\item{line_size}{Numeric value defining the line width.}

\item{line_type}{Numeric value defining the line type.}

\item{line_color}{Character value defining the line color (ignored if \code{color} is present).}

\item{line_alpha}{Numeric value defining the transparency of the line.}

\item{theme_set}{A complete ggplot2 theme.}

\item{theme_config}{A list with further arguments passed to \code{ggplot2::theme()}.}

\item{...}{Currently not in use.}
}
\value{
p An object of class ggplot.
}
\description{
Plot one or more variables as line chart.
}
