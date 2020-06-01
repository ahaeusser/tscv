
#' @title Plot the histogram of time series.
#'
#' @description Plot the historgram of one or more time series.
#'
#' @param data A valid tsibble in long format with one measurement variable.
#' @param title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param caption Caption of the plot.
#' @param bar_line_color Character value defining the outline color of the histrogram bars.
#' @param bar_fill_color Character value defining the color of the histrogram bars.
#' @param bar_alpha Numeric value defining the transparency of the histogram bars.
#' @param theme_set A complete ggplot2 theme.
#' @param theme_config A list with further arguments passed to \code{ggplot2::theme()}.
#' @param ... Further arguments passed to \code{ggplot2::geom_histogram()}.
#'
#' @return p An object of class ggplot.
#' @export

plot_histogram <- function(data,
                           title = NULL,
                           subtitle = NULL,
                           xlab = NULL,
                           ylab = NULL,
                           caption = NULL,
                           bar_line_color = "grey35",
                           bar_line_size = 0.5,
                           bar_fill_color = "grey35",
                           bar_fill_alpha = 1,
                           theme_set = theme_tscv(),
                           theme_config = list(),
                           ...) {

  response <- response_vars(data)
  value <- value_var(data)

  # Create ggplot
  p <- ggplot(
    data = data,
    aes(x = !!sym(value))
    )

  # Create grid
  p <- p + facet_wrap(
    vars(!!!syms(response)),
    scales = "free"
    )

  # Histogram
  p <- p + geom_histogram(
    aes(y = ..count..),
    na.rm = TRUE,
    color = bar_line_color,
    size = bar_line_size,
    fill = bar_fill_color,
    alpha = bar_fill_alpha,
    ...
    )

  # Axis scaling
  p <- p + scale_y_continuous()

  # Adjust annotations
  p <- p + labs(title = title)
  p <- p + labs(subtitle = subtitle)
  p <- p + labs(x = if_else(is_empty(xlab), value, xlab))
  p <- p + labs(y = if_else(is_empty(ylab), "Count", ylab))
  p <- p + labs(caption = caption)

  # Adjust ggplot2 theme
  p <- p + eval(theme_set)
  p <- p + do.call(theme, theme_config)

  return(p)
}
