
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

plot_histogram <- function(.data,
                           x,
                           facet = NULL,
                           color = NULL,
                           fill = NULL,
                           title = NULL,
                           subtitle = NULL,
                           xlab = NULL,
                           ylab = NULL,
                           caption = NULL,
                           line_color = "grey35",
                           line_width = 0.5,
                           fill_color = "grey35",
                           fill_alpha = 1,
                           theme_set = theme_tscv(),
                           theme_config = list(),
                           ...) {

  # Create initial ggplot object
  p <- ggplot(data = .data)

  # Create histogram
  if (quo_is_null(enquo(color))) {
    p <- p + geom_histogram(
      aes(
        x = {{x}},
        y = ..count..),
      na.rm = TRUE,
      color = line_color,
      fill = fill_color,
      size = line_width,
      alpha = fill_alpha,
      ...
    )
  } else {
    p <- p + geom_histogram(
      aes(
        x = {{x}},
        y = ..count..,
        color = {{color}},
        fill = {{color}}),
      na.rm = TRUE,
      size = line_width,
      alpha = fill_alpha,
      ...
    )
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
  p <- p + labs(y = if_else(is_empty(ylab), "Count", ylab))
  p <- p + labs(caption = caption)

  # Adjust ggplot2 theme
  p <- p + eval(theme_set)
  p <- p + do.call(theme, theme_config)

  return(p)
}
