
#' @title Plot time series as line chart.
#'
#' @description Plot one or more time series incl. smooth curve (linear or loess) as line chart.
#'
#' @param data A valid tsibble in long format with one measurement variable.
#' @param title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param caption Caption of the plot.
#' @param line_width Numeric value defining the line width (time series).
#' @param line_type Numeric value defining the line type (time series).
#' @param line_color Character value defining the line color (time series).
#' @param stat_method Character value. The smoothing method (\code{NULL}, \code{"loess"}, \code{"lm"}, \code{"glm"}, \code{"gam"}).
#' @param stat_width Numeric value defining the line width (smooth curve).
#' @param stat_type Numeric value defining the line type (smooth curve).
#' @param stat_color Character value defining the line color (smooth curve).
#' @param stat_fill Character value defining the fill color of confidence band.
#' @param stat_alpha Numeric value defining the transparency of confidence band.
#' @param theme_set A complete ggplot2 theme.
#' @param theme_config A list with further arguments passed to \code{ggplot2::theme()}.
#' @param ... Further arguments passed to \code{ggplot2::stat_smooth()}
#'
#' @return p An object of class ggplot.
#' @export

plot_line <- function(data,
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

  dttm <- index_var(data)
  response <- response_vars(data)
  value <- value_var(data)

  # Create ggplot
  p <- ggplot(
    data = data,
    aes(
      x = !!sym(dttm),
      y = !!sym(value))
    )

  # Create lines
  p <- p + geom_line(
    color = line_color,
    size = line_width,
    linetype = line_type,
    alpha = line_alpha
    )

  # Create smooth
  if (!is_empty(stat_method)) {
    p <- p + stat_smooth(
      method = stat_method,
      color = stat_color,
      fill = stat_fill,
      size = stat_width,
      linetype = stat_type,
      alpha = stat_alpha,
      ...
      )
  }

  # Create grid
  p <- p + facet_wrap(
    vars(!!!syms(response)),
    scales = "free"
    )

  # Axis scaling
  p <- p + scale_y_continuous()

  # Adjust annotations
  p <- p + labs(title = title)
  p <- p + labs(subtitle = subtitle)
  p <- p + labs(x = if_else(is_empty(xlab), dttm, xlab))
  p <- p + labs(y = if_else(is_empty(ylab), value, ylab))
  p <- p + labs(caption = caption)

  # Adjust ggplot2 theme
  p <- p + eval(theme_set)
  p <- p + do.call(theme, theme_config)

  return(p)
}
