
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
#' @param smooth_method Character value. The smoothing method (\code{NULL}, \code{"loess"}, \code{"lm"}, \code{"glm"}, \code{"gam"}).
#' @param smooth_se Logical value. If \code{TRUE}, confidence interval is displayed around smooth.
#' @param smooth_width Numeric value defining the line width (smooth curve).
#' @param smooth_type Numeric value defining the line type (smooth curve).
#' @param smooth_color Character value defining the line color (smooth curve).
#' @param smooth_fill Character value defining the fill color of confidence band.
#' @param smooth_alpha Numeric value defining the transparency of confidence band.
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
                      smooth_width = 0.75,
                      smooth_type = "solid",
                      smooth_color = "#D55E00",
                      line_width = 0.75,
                      line_type = "solid",
                      line_color = "grey35",
                      line_alpha = 1,
                      smooth_method = NULL,
                      smooth_fill = "grey35",
                      smooth_alpha = 0.25,
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
  if (!is.null(smooth_method)) {
    p <- p + stat_smooth(
      method = smooth_method,
      color = smooth_color,
      fill = smooth_fill,
      size = smooth_width,
      linetype = smooth_type,
      alpha = smooth_alpha,
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
