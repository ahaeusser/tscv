
#' plot_dist
#'
#' Plot the distribution of a time series (histogram, kernel density estimate, normal distribution).
#'
#' @param data A tsibble containing the columns time, variable and value.
#' @param legend Logical value. If TRUE, a legend is displayed.
#' @param legend_position Character value defining the position of the legend ("top", "bottom", "right", "left").
#' @param title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param caption Caption of the plot.
#' @param bar_color Character value defining the color of the histrogram bars
#' @param bar_alpha Numeric value defining the transparency of the histogram bars
#' @param line_width Numeric value defining the line width of the kernel density estimator
#' @param line_type Integer value defining the line type of the kernel density estimator
#' @param line_color Character value defining the line color of the kernel density estimator
#'
#' @return p An object of class ggplot
#'
#' @export

plot_dist <- function(data,
                      legend = FALSE,
                      legend_position = "right",
                      title = NULL,
                      subtitle = NULL,
                      xlab = NULL,
                      ylab = "Density",
                      caption = NULL,
                      bar_color = "#31688EFF",
                      bar_alpha = 0.3,
                      line_width = 0.1,
                      line_type = "solid",
                      line_color = "#31688EFF",
                      line_fill = "#31688EFF") {

  # Create ggplot
  p <- ggplot2::ggplot(data = data,
                       ggplot2::aes(x = value))

  # Histogram
  p <- p + ggplot2::geom_histogram(
    ggplot2::aes(y = ..density..),
    color = "white",
    fill = bar_color,
    alpha = 0.6,
    binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)))

  # Kernel density estimate
  p <- p + ggplot2::geom_density(
    ggplot2::aes(colour = "Kernel"),
                 linetype = line_type,
                 size = line_width,
                 fill = line_fill,
                 alpha = 0.3)

  # Create grid
  p <- p + ggplot2::facet_wrap(~variable, scales = "free")

  # Color
  p <- p + ggplot2::scale_colour_manual("", values = c(line_color, "black"))

  # Axis scaling
  p <- p + ggplot2::scale_y_continuous()
  # Adjust annotations
  p <- p + ggplot2::labs(title = title)
  p <- p + ggplot2::labs(subtitle = subtitle)
  p <- p + ggplot2::labs(x = xlab)
  p <- p + ggplot2::labs(y = ylab)
  p <- p + ggplot2::labs(caption = caption)
  # Adjust theme
  p <- p + theme_tscv(10)

  if (legend == FALSE) {
    p <- p + ggplot2::theme(legend.position = "none")
  } else {
    p <- p + ggplot2::theme(legend.position = legend_position)
  }
  return(p)

}
