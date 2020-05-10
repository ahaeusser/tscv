
#' @title Plot partial autocorrelation function.
#'
#' @description This function plots the sample partial autocorrelation function of one or more time series.
#'
#' @param data A tsibble containing the columns time, variable and value.
#' @param lag_max Integer value. Maximum number of lags.
#' @param level Numeric value. The confidence level to check significance.
#' @param title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param caption Caption of the plot.
#' @param bar_width Numeric value. The width of the bars.
#' @param bar_color Character value. The color of the significant bars.
#' @param bar_color2 Character value. The color of the non-significant bars.
#' @param bar_alpha Numeric value. The transparency of the bars.
#' @param line_width Numeric value. Line width of the confidence line.
#' @param line_type Character value. Line type of the confidence line.
#' @param line_color Character value. Line color of the confidence line.
#' @param legend Logical value. If \code{TRUE}, a legend is plotted.
#' @param legend_position Character value. The legend position ("top, "bottom", "right", "left").
#' @param theme_ggplot2 A complete ggplot2 theme.
#' @param theme_config A list with further arguments passed to \code{ggplot2::theme()}.
#'
#' @return p An object of class ggplot.
#' @export

plot_pacf <- function(data,
                     lag_max = 25,
                     level = 0.9,
                     title = "Correlation analysis",
                     subtitle = "Sample partial autocorrelation (PACF)",
                     xlab = "Lag k",
                     ylab = NULL,
                     caption = NULL,
                     bar_width = 1,
                     bar_color = "#31688EFF",
                     bar_color2 = "#D55E00",
                     bar_alpha = 0.6,
                     line_width = 0.25,
                     line_type = "solid",
                     line_color = "#31688EFF",
                     legend = TRUE,
                     legend_position = "bottom",
                     theme_ggplot2 = theme_gray(),
                     theme_config = list()) {

  # Calculate confidence interval
  n_obs <- data %>%
    spread(
      key = "variable",
      value = "value") %>%
    nrow()

  ci_line <- qnorm((1 - level) / 2) / sqrt(n_obs)

  # Estimate sample partial autocorrelation function
  data <- data %>%
    PACF(value, lag_max = lag_max) %>%
    as_tibble() %>%
    rename(PACF = pacf) %>%
    select(variable, PACF) %>%
    gather(key = "type",
           value = "value",
           -variable)

  # Prepare data for plotting
  data <- data %>%
    group_by(variable) %>%
    mutate(lag = row_number()) %>%
    mutate(sign = ifelse(abs(value) > abs(ci_line), TRUE, FALSE)) %>%
    ungroup()

  # Create ggplot
  p <- ggplot(
    data = data,
    aes(
      x = lag,
      y = value,
      fill = sign))

  # Bars for autocorrelation
  p <- p + geom_bar(
    stat = "identity",
    position = "identity",
    alpha = bar_alpha,
    width = bar_width)

  # Create grid
  p <- p + facet_grid(
    vars(variable),
    scales = "free")

  # Color bars depending on significance
  if (all(data$sign == FALSE)) {
    p <- p + scale_fill_manual(values = c(bar_color2))
  } else if (all(data$sign == TRUE)) {
    p <- p + scale_fill_manual(values = c(bar_color))
  } else {
    p <- p + scale_fill_manual(values = c(bar_color2, bar_color))
  }

  # Lower confidence interval
  p <- p + geom_hline(
    yintercept = -ci_line,
    color = line_color,
    size = line_width,
    linetype = line_type)

  # Upper confidence interval
  p <- p + geom_hline(
    yintercept = ci_line,
    color = line_color,
    size = line_width,
    linetype = line_type)

  # Adjust annotations
  p <- p + labs(title = title)
  p <- p + labs(subtitle = subtitle)
  p <- p + labs(x = xlab)
  p <- p + labs(y = ylab)
  p <- p + labs(caption = caption)

  # Adjust ggplot2 theme
  p <- p + eval(theme_ggplot2)
  p <- p + do.call(theme, theme_config)

  # Adjust legend
  if (legend == FALSE) {
    p <- p + ggplot2::theme(legend.position = "none")
  } else {
    p <- p + ggplot2::theme(legend.position = legend_position)
  }

  return(p)
}
