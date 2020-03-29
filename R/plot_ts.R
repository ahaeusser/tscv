#' @title Plot time series as line chart.
#'
#' @description Line chart of one or more time series incl. smooth curve (linear or loess).
#'
#' @param data A tsibble containing the columns time, variable and value.
#' @param smooth_method Character value defining the smoothing method ("loess", "lm" or NULL).
#' @param smooth_se Logical value. If TRUE, confidence interval is displayed around smooth.
#' @param title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param caption Caption of the plot.
#' @param line_width Numeric value defining the line width (time series).
#' @param line_type Numeric value defining the line type (time series).
#' @param line_color Character value defining the line color (time series).
#' @param smooth_width Numeric value defining the line width (smooth curve).
#' @param smooth_type Numeric value defining the line type (smooth curve).
#' @param smooth_color Character value defining the line color (smooth curve).
#' @param smooth_fill Character value defining the fill color of confidence band.
#' @param smooth_alpha Numeric value defining the transparency of confidence band.
#' @param base_size Integer value. Base font size.
#'
#' @return p An object of class ggplot.
#' @export

plot_ts <- function(data,
                    smooth_method = NULL,
                    smooth_se = FALSE,
                    title = NULL,
                    subtitle = NULL,
                    xlab = "Time",
                    ylab = NULL,
                    caption = NULL,
                    smooth_width = 0.5,
                    smooth_type = "solid",
                    smooth_color = "#FDE725FF",
                    line_width = 0.75,
                    line_type = "solid",
                    line_color = "#31688EFF",
                    line_alpha = 1,
                    smooth_fill = "#FDE725FF",
                    smooth_alpha = 0.25,
                    base_size = 10) {

  # Create ggplot
  p <- ggplot(
    data = data,
    aes(x = date_time,
        y = value))

  # Create lines
  p <- p + geom_line(
    color = line_color,
    size = line_width,
    linetype = line_type,
    alpha = line_alpha)

  # Create smooth
  if (!is.null(smooth_method)) {
    p <- p + geom_smooth(
      method = smooth_method,
      se = smooth_se,
      color = smooth_color,
      fill = smooth_fill,
      size = smooth_width,
      linetype = smooth_type,
      alpha = smooth_alpha)
  }

  # Create grid
  p <- p + facet_wrap(~variable, scales = "free")

  # Axis scaling
  p <- p + scale_y_continuous()
  # Adjust annotations
  p <- p + labs(title = title)
  p <- p + labs(subtitle = subtitle)
  p <- p + labs(x = xlab)
  p <- p + labs(y = ylab)
  p <- p + labs(caption = caption)
  # Adjust theme
  p <- p + theme_tscv(base_size = base_size)
  p <- p + theme(legend.position = "none")
  return(p)

}
