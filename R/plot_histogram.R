
#' @title Plot the histogram of time series.
#'
#' @description Plot the historgram of one or more time series.
#'
#' @param data A tsibble containing the columns time, variable and value.
#' @param title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param caption Caption of the plot.
#' @param bar_color Character value defining the color of the histrogram bars.
#' @param bar_alpha Numeric value defining the transparency of the histogram bars.
#' @param theme_ggplot2 A complete ggplot2 theme.
#' @param theme_config A list with further arguments passed to \code{ggplot2::theme()}.
#'
#' @return p An object of class ggplot.
#' @export

plot_histogram <- function(data,
                           title = NULL,
                           subtitle = NULL,
                           xlab = NULL,
                           ylab = NULL,
                           caption = NULL,
                           bar_color = "#31688EFF",
                           bar_alpha = 0.5,
                           theme_ggplot2 = theme_gray(),
                           theme_config = list()) {

  variable <- key_vars(data)
  value <- measured_vars(data)

  # Create ggplot
  p <- ggplot(
    data = data,
    aes(x = !!sym(value)))

  # Histogram
  p <- p + geom_histogram(
    aes(y = ..density..),
    fill = bar_color,
    alpha = bar_alpha,
    binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)))

  # Create grid
  p <- p + facet_wrap(
    vars(!!sym(variable)),
    scales = "free")

  # Axis scaling
  p <- p + scale_y_continuous()

  # Adjust annotations
  p <- p + labs(title = title)
  p <- p + labs(subtitle = subtitle)
  p <- p + labs(x = xlab)
  p <- p + labs(y = ylab)
  p <- p + labs(caption = caption)

  # Adjust ggplot2 theme
  p <- p + eval(theme_ggplot2)
  p <- p + do.call(theme, theme_config)

  return(p)

}
