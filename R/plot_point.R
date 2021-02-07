
#' @title Plot data as scatterplot
#'
#' @description Plot one or more variables as scatterplot.
#'
#' @param data A \code{data.frame}, \code{tibble} or \code{tsibble} in long format.
#' @param x Unquoted column within \code{.data}.
#' @param y Unquoted column within \code{.data} containing numeric values.
#' @param facet_var Unquoted column within \code{.data} (facet).
#' @param facet_scale Character value defining axis scaling (\code{facet_var = "free"} or \code{facet_var = "fixed"}).
#' @param color Unquoted column within \code{.data} (color).
#' @param title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param caption Caption of the plot.
#' @param point_size Numeric value defining the point size.
#' @param point_type Numeric value defining the point type.
#' @param point_color Character value defining the point color (ignored if \code{color} is present).
#' @param point_alpha Numeric value defining the transparency of the points.
#' @param theme_set A complete ggplot2 theme.
#' @param theme_config A list with further arguments passed to \code{ggplot2::theme()}.
#' @param ... Currently not in use.
#'
#' @return p An object of class ggplot.
#' @export

plot_point <- function(data,
                       x,
                       y,
                       facet_var = NULL,
                       facet_scale = "free",
                       color = NULL,
                       title = NULL,
                       subtitle = NULL,
                       xlab = NULL,
                       ylab = NULL,
                       caption = NULL,
                       point_size = 1.5,
                       point_type = 16,
                       point_color = "grey35",
                       point_alpha = 1,
                       theme_set = theme_tscv(),
                       theme_config = list(),
                       ...) {

  # Create initial ggplot object
  p <- ggplot(data = data)

  # Create points
  if (quo_is_null(enquo(color))) {
    p <- p + geom_point(
      aes(
        x = {{x}},
        y = {{y}}),
      color = point_color,
      size = point_size,
      shape = point_type,
      alpha = point_alpha
    )
  } else {
    p <- p + geom_point(
      aes(
        x = {{x}},
        y = {{y}},
        color = {{color}}),
      size = point_size,
      shape = point_type,
      alpha = point_alpha
    )
  }

  # Create facet
  if (!quo_is_null(enquo(facet_var))) {
    p <- p + facet_wrap(
      vars({{facet_var}}),
      scales = facet_scale
    )
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
