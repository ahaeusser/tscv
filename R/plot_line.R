
#' @title Plot time series as line chart
#'
#' @description Plot one or more time series incl. smooth curve (linear or loess) as line chart.
#'
#' @param data A \code{data.frame}, \code{tibble} or \code{tsibble} in long format.
#' @param x Unquoted column within \code{.data}.
#' @param y Unquoted column within \code{.data} containing numeric values.
#' @param facet Unquoted column within \code{.data} (facet).
#' @param color Unquoted column within \code{.data} (color).
#' @param title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param caption Caption of the plot.
#' @param line_width Numeric value defining the line width (time series).
#' @param line_type Numeric value defining the line type (time series).
#' @param line_color Character value defining the line color (ignored if \code{color} is present).
#' @param line_alpha Numeric value defining the transparency of the line.
#' @param stat_method Character value. The smoothing method (\code{NULL}, \code{"loess"}, \code{"lm"}, \code{"glm"}, \code{"gam"}).
#' @param stat_width Numeric value defining the line width (smooth curve).
#' @param stat_type Numeric value defining the line type (smooth curve).
#' @param stat_color Character value defining the line color (ignored if \code{color} is present).
#' @param stat_fill Character value defining the fill color of confidence band.
#' @param stat_alpha Numeric value defining the transparency of confidence band.
#' @param theme_set A complete ggplot2 theme.
#' @param theme_config A list with further arguments passed to \code{ggplot2::theme()}.
#' @param ... Further arguments passed to \code{ggplot2::stat_smooth()}
#'
#' @return p An object of class ggplot.
#' @export

plot_line <- function(data,
                      x,
                      y,
                      facet = NULL,
                      color = NULL,
                      title = NULL,
                      subtitle = NULL,
                      xlab = NULL,
                      ylab = NULL,
                      caption = NULL,
                      line_width = 0.75,
                      line_type = "solid",
                      line_color = "grey35",
                      line_alpha = 1,
                      stat_method = NULL,
                      stat_width = 0.75,
                      stat_type = "solid",
                      stat_color = "#D55E00",
                      stat_fill = "grey35",
                      stat_alpha = 0.25,
                      theme_set = theme_tscv(),
                      theme_config = list(),
                      ...) {

  # Create initial ggplot object
  p <- ggplot(data = data)

  # Create line
  if (quo_is_null(enquo(color))) {
    p <- p + geom_line(
      aes(
        x = {{x}},
        y = {{y}}),
      color = line_color,
      size = line_width,
      linetype = line_type,
      alpha = line_alpha
    )
  } else {
    p <- p + geom_line(
      aes(
        x = {{x}},
        y = {{y}},
        color = {{color}}),
      size = line_width,
      linetype = line_type,
      alpha = line_alpha
    )
  }

  # Create smooth
  if (!is_empty(stat_method)) {
    if (quo_is_null(enquo(color))) {
      p <- p + geom_smooth(
        aes(
          x = {{x}},
          y = {{y}}),
        method = stat_method,
        color = stat_color,
        size = stat_width,
        linetype = stat_type,
        alpha = stat_alpha,
        ...
      )
    } else {
      p <- p + geom_smooth(
        aes(
          x = {{x}},
          y = {{y}},
          color = {{color}}),
        method = stat_method,
        size = stat_width,
        linetype = stat_type,
        alpha = stat_alpha,
        ...
      )
    }
  }

  # Create facet
  if (!quo_is_null(enquo(facet))) {
    p <- p + facet_wrap(
      vars({{facet}}),
      scales = "free"
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
