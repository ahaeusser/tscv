
#' @title Quantile-Quantile plot (normal).
#'
#' @description Quantile-Quantile plot (normal) of one or more time series.
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
#'
#' @return p An object of class ggplot
#' @export

plot_qq <- function(data,
                    title = NULL,
                    subtitle = NULL,
                    xlab = "Theoretical quantile",
                    ylab = "Sample quantile",
                    caption = NULL,
                    point_size = 2,
                    point_shape = 16,
                    point_color = "#31688EFF",
                    point_fill = "#31688EFF",
                    point_alpha = 0.25,
                    line_width = 0.25,
                    line_type = "solid",
                    line_color = "grey35",
                    theme_set = theme_gray(),
                    theme_config = list()) {

  variable <- key_vars(data)
  value <- measured_vars(data)

  # Create ggplot
  p <- ggplot(
    data = data,
    aes(sample = !!sym(value)))

  # Create points
  p <- p + stat_qq(
    color = point_color,
    fill = point_fill,
    alpha = point_alpha,
    size = point_size,
    shape = point_shape)

  # Create 45-degree line
  p <- p + stat_qq_line(
    color = line_color,
    size = line_width,
    linetype = line_type)

  # Create grid
  p <- p + facet_wrap(
    vars(!!!syms(variable)),
    scales = "free")

  # Axis scaling
  p <- p + scale_x_continuous()
  p <- p + scale_y_continuous()

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
