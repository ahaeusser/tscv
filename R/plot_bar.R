
#' @title Plot data as bar chart
#'
#' @description Plot one or more variables as bar chart.
#'
#' @param data A \code{data.frame}, \code{tibble} or \code{tsibble} in long format.
#' @param x Unquoted column within \code{.data}.
#' @param y Unquoted column within \code{.data} containing numeric values.
#' @param facet_var Unquoted column within \code{.data} (facet).
#' @param facet_scale Character value defining axis scaling. Possible values are \code{"free"}, \code{"fixed"}, \code{"free_x"} and \code{"free_y"}.
#' @param facet_nrow Integer value. The number of rows.
#' @param facet_ncol Integer value. The number of columns.
#' @param color Unquoted column within \code{.data} (color).
#' @param flip Logical value. If \code{TRUE}, the plot is flipped from horizontal to vertical.
#' @param title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param caption Caption of the plot.
#' @param bar_size Numeric value defining the line width.
#' @param bar_color Character value defining the line color (ignored if \code{color} is present).
#' @param bar_alpha Numeric value defining the transparency of the line.
#' @param theme_set A complete ggplot2 theme.
#' @param theme_config A list with further arguments passed to \code{ggplot2::theme()}.
#' @param ... Currently not in use.
#'
#' @return p An object of class ggplot.
#' @export

plot_bar <- function(data,
                     x,
                     y,
                     facet_var = NULL,
                     facet_scale = "free",
                     facet_nrow = NULL,
                     facet_ncol = NULL,
                     color = NULL,
                     flip = FALSE,
                     title = NULL,
                     subtitle = NULL,
                     xlab = NULL,
                     ylab = NULL,
                     caption = NULL,
                     bar_size = 0.75,
                     bar_color = "grey35",
                     bar_alpha = 1,
                     theme_set = theme_tscv(),
                     theme_config = list(),
                     ...) {

  # Create initial ggplot object
  p <- ggplot(data = data)

  # Create line
  if (quo_is_null(enquo(color))) {
    p <- p + geom_bar(
      stat = "identity",
      aes(
        x = {{x}},
        y = {{y}}),
      fill = bar_color,
      size = bar_size,
      alpha = bar_alpha
    )
  } else {
    p <- p + geom_bar(
      stat = "identity",
      aes(
        x = {{x}},
        y = {{y}},
        fill = {{color}}),
      size = bar_size,
      alpha = bar_alpha
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
   p <- p + scale_x_reordered()
  }

  # Flip plot (from horizontal to vertical)
  if (flip == TRUE) {
    p <- p + coord_flip()
  }

  # Adjust annotations
  p <- p + labs(title = title)
  p <- p + labs(subtitle = subtitle)
  p <- p + labs(x = if_else(is_empty(xlab), as_name(enquo(x)), xlab))
  p <- p + labs(y = if_else(is_empty(ylab), as_name(enquo(y)), ylab))
  p <- p + labs(caption = caption)

  # Adjust ggplot2 theme
  p <- p + eval(theme_set)
  p <- p + do.call(theme, theme_config)

  return(p)
}
