
#' @title Quantile-Quantile plot.
#'
#' @description Quantile-Quantile plot of one or more time series.
#'
#' @param data A valid tsibble in long format with one measurement variable.
#' @param title Title of the plot (default select.target)
#' @param subtitle Subtitle of the plot (default NULL)
#' @param xlab Label for the x-axis
#' @param ylab Label for the y-axis
#' @param caption Caption of the plot
#' @param point_size Numeric value defining the point size
#' @param point_shape Integer value defining the point shape
#' @param point_color Character value defining the point color
#' @param point_alpha Numeric value defining the transperency of points
#' @param line_width Numeric value defining the line width (45-degree line)
#' @param line_type Integer value defining the line type (45-degree line)
#' @param line_color Character value defining the line color (45-degree line)
#' @param theme_set A complete ggplot2 theme.
#' @param theme_config A list with further arguments passed to \code{ggplot2::theme()}.
#' @param ... Further arguments passed to \code{qqplotr::stat_qq_point()},
#'    \code{qqplotr::stat_qq_line()}, \code{qqplotr::stat_qq_band()}.
#'
#' @return p An object of class ggplot
#' @export

plot_qq <- function(.data,
                    x,
                    facet = NULL,
                    color = NULL,
                    title = NULL,
                    subtitle = NULL,
                    xlab = NULL,
                    ylab = NULL,
                    caption = NULL,
                    point_size = 2,
                    point_shape = 16,
                    point_color = "grey35",
                    point_fill = "grey35",
                    point_alpha = 0.25,
                    line_width = 0.25,
                    line_type = "solid",
                    line_color = "grey35",
                    line_alpha = 1,
                    band_color = "grey35",
                    band_alpha = 0.25,
                    theme_set = theme_tscv(),
                    theme_config = list(),
                    ...) {

  # Create initial ggplot object
  p <- ggplot(data = .data)

  # Create density plot
  if (quo_is_null(enquo(color))) {
    # Create points
    p <- p + stat_qq_point(
      aes(sample = {{x}}),
      color = point_color,
      fill = point_fill,
      alpha = point_alpha,
      size = point_size,
      shape = point_shape,
      ...)

    # Create line
    p <- p + stat_qq_line(
      aes(sample = {{x}}),
      color = line_color,
      size = line_width,
      linetype = line_type,
      alpha = line_alpha,
      ...)

    # Create confidence bands
    p <- p + stat_qq_band(
      aes(sample = {{x}}),
      bandType = "pointwise",
      fill = band_color,
      alpha = band_alpha,
      ...)
  } else {
    # Create points
    p <- p + stat_qq_point(
      aes(
        sample = {{x}},
        color = {{color}},
        fill = {{color}}),
      alpha = point_alpha,
      size = point_size,
      shape = point_shape,
      ...)

    # Create line
    p <- p + stat_qq_line(
      aes(
        sample = {{x}},
        color = {{color}}),
      size = line_width,
      linetype = line_type,
      alpha = line_alpha,
      ...)

    # Create confidence bands
    p <- p + stat_qq_band(
      aes(
        sample = {{x}},
        fill = {{color}}),
      bandType = "pointwise",
      alpha = band_alpha,
      ...)
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
  p <- p + labs(x = if_else(is_empty(xlab), "Theoretical quantile", xlab))
  p <- p + labs(y = if_else(is_empty(ylab), "Sample quantile", ylab))
  p <- p + labs(caption = caption)

  # Adjust ggplot2 theme
  p <- p + eval(theme_set)
  p <- p + do.call(theme, theme_config)

  return(p)
}
