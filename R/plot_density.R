
#' @title Plot the density via Kernel Density Estimator
#'
#' @description Plot the density of one or more time series via Kernel Density Estimator.
#'
#' @param data A \code{data.frame}, \code{tibble} or \code{tsibble} in long format.
#' @param x Unquoted column within \code{.data} containing numeric values.
#' @param facet_var Unquoted column within \code{.data} (facet).
#' @param facet_scale Character value defining axis scaling (\code{facet_var = "free"} or \code{facet_var = "fixed"}).
#' @param facet_nrow Integer value. The number of rows.
#' @param facet_ncol Integer value. The number of columns.
#' @param color Unquoted column within \code{.data} (color).
#' @param fill Unquoted column within \code{.data} (fill color).
#' @param title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param caption Caption of the plot.
#' @param line_width Numeric value defining the line width of the kernel density estimator.
#' @param line_type Integer value defining the line type of the kernel density estimator.
#' @param line_color Character value defining the line color of the kernel density estimator.
#' @param fill_color Character value defining the fill color for the area under the kernel density estimator.
#' @param fill_alpha Numeric value defining the transparency of the area under the kernel density estimator.
#' @param theme_set A complete ggplot2 theme.
#' @param theme_config A list with further arguments passed to \code{ggplot2::theme()}.
#' @param ... Further arguments passed to \code{geom_density()}.
#'
#' @return p An object of class ggplot.
#' @export

plot_density <- function(data,
                         x,
                         facet_var = NULL,
                         facet_scale = "free",
                         facet_nrow = NULL,
                         facet_ncol = NULL,
                         color = NULL,
                         fill = NULL,
                         title = NULL,
                         subtitle = NULL,
                         xlab = NULL,
                         ylab = NULL,
                         caption = NULL,
                         line_width = 0.1,
                         line_type = "solid",
                         line_color = "grey35",
                         fill_color = "grey35",
                         fill_alpha = 0.5,
                         theme_set = theme_tscv(),
                         theme_config = list(),
                         ...) {

  # Create initial ggplot object
  p <- ggplot(data = data)

  # Create density plot
  if (quo_is_null(enquo(color))) {
    p <- p + geom_density(
      aes(x = {{x}}),
      na.rm = TRUE,
      color = line_color,
      fill = fill_color,
      size = line_width,
      alpha = fill_alpha,
      ...
    )
  } else {
    p <- p + geom_density(
      aes(
        x = {{x}},
        color = {{color}},
        fill = {{color}}),
      na.rm = TRUE,
      size = line_width,
      alpha = fill_alpha,
      ...
    )
  }

  # Create facet
  if (!quo_is_null(enquo(facet_var))) {
    p <- p + facet_wrap(
      facets = vars({{facet_var}}),
      scales = facet_scale,
      nrow = facet_nrow,
      ncol = facet_ncol
    )
  }

  # Adjust annotations
  p <- p + labs(title = title)
  p <- p + labs(subtitle = subtitle)
  p <- p + labs(x = xlab)
  p <- p + labs(y = ylab)
  p <- p + labs(caption = caption)

  # Adjust ggplot2 theme
  p <- p + eval(theme_set)
  p <- p + do.call(theme, theme_config)

  return(p)
}
