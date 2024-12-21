
#' @title Plot data as histogram
#'
#' @description Plot one or more variables as histogram.
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
#' @param line_color Character value defining the outline color of the histogram bars.
#' @param line_width Numeric value defining the outline width of the histogram bars.
#' @param fill_color Character value defining the color of the histogram bars.
#' @param fill_alpha Numeric value defining the transparency of the histogram bars.
#' @param theme_set A complete ggplot2 theme.
#' @param theme_config A list with further arguments passed to \code{ggplot2::theme()}.
#' @param ... Further arguments passed to \code{ggplot2::geom_histogram()}.
#'
#' @return p An object of class ggplot.
#' @export

plot_histogram <- function(data,
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
                           line_color = "grey35",
                           line_width = 0.5,
                           fill_color = "grey35",
                           fill_alpha = 1,
                           theme_set = theme_tscv(),
                           theme_config = list(),
                           ...) {

  # Create initial ggplot object
  p <- ggplot(data = data)

  # Create histogram
  if (quo_is_null(enquo(color))) {
    p <- p + geom_histogram(
      aes(
        x = {{x}}),
      na.rm = TRUE,
      color = line_color,
      fill = fill_color,
      size = line_width,
      alpha = fill_alpha,
      ...
    )
  } else {
    p <- p + geom_histogram(
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
