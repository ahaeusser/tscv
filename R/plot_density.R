
#' @title Plot the density via Kernel Density Estimator.
#'
#' @description Plot the density of one or more time series via Kernel Density Estimator.
#'
#' @param data A valid tsibble in long format with one measurement variable.
#' @param title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param caption Caption of the plot.
#' @param line_width Numeric value defining the line width of the kernel density estimator.
#' @param line_type Integer value defining the line type of the kernel density estimator.
#' @param line_color Character value defining the line color of the kernel density estimator.
#' @param line_fill Character value defining the fill color for the area under the kernel density estimator.
#' @param line_alpha Numeric value defining the transparency of the area under the kernel density estimator.
#' @param theme_set A complete ggplot2 theme.
#' @param theme_config A list with further arguments passed to \code{ggplot2::theme()}.
#'
#' @return p An object of class ggplot.
#' @export

plot_density <- function(data,
                         title = NULL,
                         subtitle = NULL,
                         xlab = NULL,
                         ylab = NULL,
                         caption = NULL,
                         line_width = 0.1,
                         line_type = "solid",
                         line_color = "#31688EFF",
                         line_fill = "#31688EFF",
                         line_alpha = 0.5,
                         theme_set = theme_tscv(),
                         theme_config = list()) {

  variable <- key_vars(data)
  value <- measured_vars(data)

  # Create ggplot
  p <- ggplot(
    data = data,
    aes(x = !!sym(value)))

  # Kernel density estimate
  p <- p + geom_density(
    aes(colour = "Kernel"),
    # linetype = line_type,
    # size = line_width,
    fill = line_fill,
    alpha = line_alpha,
    color = line_color)

  # Create grid
  p <- p + facet_wrap(
    vars(!!!syms(variable)),
    scales = "free")

  # Axis scaling
  p <- p + scale_y_continuous()

  # Adjust annotations
  p <- p + labs(title = title)
  p <- p + labs(subtitle = subtitle)
  p <- p + labs(x = if_else(is_empty(xlab), value, xlab))
  p <- p + labs(y = if_else(is_empty(ylab), "Density", ylab))
  p <- p + labs(caption = caption)

  # Adjust ggplot2 theme
  p <- p + eval(theme_set)
  p <- p + do.call(theme, theme_config)

  return(p)
}
