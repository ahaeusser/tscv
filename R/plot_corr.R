#' @title Plot autocorrelation and partial autocorrelation function.
#'
#' @description This function plots the sample autocorrelation and partial autocorrelation function.
#'
#' @param data A tsibble containing the columns time, variable and value.
#' @param lag_max Integer value. Maximum number of lags.
#' @param demean Logical value. If \code{TRUE}, the time series is demeaned.
#' @param level Numeric value. The confidence level to check significance.
#' @param title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param caption Caption of the plot.
#' @param bar_width Numeric value. The width of the bars.
#' @param bar_color Character value. The color of the (significant) bars.
#' @param bar_alpha Numeric value. The transparency of the bars.
#' @param line_width Numeric value. Line width of the confidence line.
#' @param line_type Character value. Line type of the confidence line.
#' @param line_color Character value. Line color of the confidence line.
#' @param legend Logical value. If \code{TRUE}, a legend is plotted.
#' @param legend_position Character value. The legend position ("top, "bottom", "right", "left").
#' @param base_size Integer value. Base font size.
#'
#' @return p An object of class ggplot.
#' @export

plot_corr <- function(data,
                      lag_max = 48,
                      demean = TRUE,
                      level = 0.9,
                      title = "Correlation analysis",
                      subtitle = "Sample autocorrelation (ACF) and partial autocorrelation (PACF)",
                      xlab = "Lag k",
                      ylab = NULL,
                      caption = NULL,
                      bar_width = 1,
                      bar_color = "#31688EFF",
                      bar_alpha = 0.6,
                      line_width = 0.25,
                      line_type = "solid",
                      line_color = "grey35",
                      legend = TRUE,
                      legend_position = "bottom",
                      base_size = 10) {

  # Calculate confidence interval
  n_obs <- data %>%
    spread(
      key = "variable",
      value = "value") %>%
    nrow()

  ci_line <- qnorm((1 - level) / 2) / sqrt(n_obs)

  # Estimate sample autocorrelation function
  acf <- data %>%
    ACF(value, lag_max = lag_max, demean = demean) %>%
    as_tibble() %>%
    rename(ACF = acf) %>%
    select(variable, ACF) %>%
    gather(key = "type",
           value = "value",
           -variable)

  # Estimate sample partial autocorrelation function
  pacf <- data %>%
    PACF(value, lag_max = lag_max) %>%
    as_tibble() %>%
    rename(PACF = pacf) %>%
    select(variable, PACF) %>%
    gather(key = "type",
           value = "value",
           -variable)

  # Prepare data for plotting
  corr <- bind_rows(acf, pacf) %>%
    group_by(variable, type) %>%
    mutate(lag = row_number()) %>%
    mutate(sign = ifelse(abs(value) > abs(ci_line), TRUE, FALSE)) %>%
    ungroup()

  # Create ggplot
  p <- ggplot(
    data = corr,
    aes(
      x = lag,
      y = value,
      fill = sign))

  # Bars for autocorrelation
  p <- p + ggplot2::geom_bar(
    stat = "identity",
    position = "identity",
    alpha = bar_alpha,
    width = bar_width,
    color = "white")

  # Faceting by .variable and .slice
  p <- p + facet_grid(
    vars(variable),
    vars(type),
    scales = "free")

  # Color bars depending on significance
  if (all(corr$sign == FALSE)) {
    p <- p + scale_fill_manual(values = c("grey35"))
  } else if (all(corr$sign == TRUE)) {
    p <- p + scale_fill_manual(values = c(bar_color))
  } else {
    p <- p + scale_fill_manual(values = c("grey35",bar_color))
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
  p <- p + theme_tscv(base_size = base_size)

  # Adjust legend
  if (legend == FALSE) {
    p <- p + ggplot2::theme(legend.position = "none")
  } else {
    p <- p + ggplot2::theme(legend.position = legend_position)
  }
  return(p)
}
