#' @title Quantile-Quantile plot (normal)
#'
#' @description Quantile-Quantile plot (normal) of one or more time series.
#'
#' @param data A tsibble containing the columns time, variable and value.
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
#' @param base_size Integer value. Base font size
#'
#' @return p An object of class ggplot
#' @export

plot_qq <- function(data,
                    title = NULL,
                    subtitle = NULL,
                    xlab = "Theoretical quantile",
                    ylab = "Sample quantile",
                    caption = NULL,
                    point_size = 4,
                    point_shape = 16,
                    point_color = "#31688EFF",
                    point_fill = "#31688EFF",
                    point_alpha = 0.5,
                    line_width = 0.25,
                    line_type = "solid",
                    line_color = "grey35",
                    base_size = 11) {

  # Create ggplot
  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(sample = value))

  # Create points
  p <- p + ggplot2::stat_qq(
    color = point_color,
    fill = point_fill,
    alpha = point_alpha,
    size = point_size,
    shape = point_shape)

  # Create 45-degree line
  p <- p + ggplot2::stat_qq_line(
    color = line_color,
    size = line_width,
    linetype = line_type)

  # Create grid
  p <- p + ggplot2::facet_wrap(~variable, scales = "free")

  # Axis scaling
  p <- p + ggplot2::scale_x_continuous()
  p <- p + ggplot2::scale_y_continuous()
  # Adjust annotations
  p <- p + ggplot2::labs(title = title)
  p <- p + ggplot2::labs(subtitle = subtitle)
  p <- p + ggplot2::labs(x = xlab)
  p <- p + ggplot2::labs(y = ylab)
  p <- p + ggplot2::labs(caption = caption)
  # Adjust theme
  p <- p + theme_tscv(base_size = base_size)
  return(p)
}
