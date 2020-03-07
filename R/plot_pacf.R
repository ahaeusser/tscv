#' @title Plot the sample partial autocorrelation function (PACF).
#'
#' @description Bar chart of the sample partial autocorrelation function (PACF) of a time series.
#'
#' @param data A tsibble containing the columns time, variable and value.
#' @param lag_max Integer value defining the maximum number of lags.
#' @param level Numeric value defining the confidence level.
#' @param title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param caption Caption of the plot.
#' @param bar_width Numeric value defining the bar width.
#' @param bar_color Character value defining the bar color.
#' @param line_width Numeric value defining the line width.
#' @param line_type Integer value defining the line type.
#' @param line_color Character value defining the line color.
#' @param legend Logical value. If \code{TRUE}, a legend is plotted.
#' @param legend_position Character value defining the position of the legend ("top", "bottom", "right", "left").
#'
#' @return p An object of class ggplot.
#' @export

plot_pacf <- function(data,
                      lag_max = 24,
                      level = 0.9,
                      title = NULL,
                      subtitle = NULL,
                      xlab = "Lag k",
                      ylab = "PACF",
                      caption = NULL,
                      bar_width = 1,
                      bar_color = "#31688EFF",
                      bar_alpha = 0.6,
                      line_width = 0.25,
                      line_type = "solid",
                      line_color = "grey35",
                      legend = FALSE,
                      legend_position = "right") {

  # Extract values
  y <- data %>% dplyr::pull(value)
  # Calculate confidence interval
  ci_line <- qnorm((1 - level) / 2) / sqrt(length(y))

  # Sample autocorrelation function
  pacf <- stats::pacf(y, plot = FALSE, lag.max = lag_max)
  pacf <- tibble::tibble(lag = pacf$lag[, , 1],
                         pacf = pacf$acf[, , 1])

  # Prepare data for plotting
  pacf <- pacf %>%
    dplyr::mutate(sign = ifelse(
      abs(pacf) > abs(ci_line), TRUE, FALSE))

  # Create ggplot
  p <- ggplot2::ggplot(data = pacf,
                       mapping = ggplot2::aes(
                         x = lag,
                         y = pacf,
                         fill = sign))

  # Bars for autocorrelation
  p <- p + ggplot2::geom_bar(
    stat = "identity",
    position = "identity",
    alpha = bar_alpha,
    width = bar_width,
    color = "white")

  # Color bars depending on significance
  if (all(pacf$sign == FALSE)) {
    p <- p + ggplot2::scale_fill_manual(values = c("grey35"))
  } else if (all(pacf$sign == TRUE)) {
    p <- p + ggplot2::scale_fill_manual(values = c(bar_color))
  } else {
    p <- p + ggplot2::scale_fill_manual(values = c("grey35",bar_color))
  }

  # Lower confidence interval
  p <- p + ggplot2::geom_hline(
    yintercept = -ci_line,
    color = line_color,
    size = line_width,
    linetype = line_type)

  # Upper confidence interval
  p <- p + ggplot2::geom_hline(
    yintercept = ci_line,
    color = line_color,
    size = line_width,
    linetype = line_type)

  # Adjust annotations
  p <- p + ggplot2::labs(title = title)
  p <- p + ggplot2::labs(subtitle = subtitle)
  p <- p + ggplot2::labs(x = xlab)
  p <- p + ggplot2::labs(y = ylab)
  p <- p + ggplot2::labs(caption = caption)
  # Adjust theme
  p <- p + theme_tscv(10)
  # Adjust legend
  if (legend == FALSE) {
    p <- p + ggplot2::theme(legend.position = "none")
  } else {
    p <- p + ggplot2::theme(legend.position = legend_position)
  }
  return(p)
}
