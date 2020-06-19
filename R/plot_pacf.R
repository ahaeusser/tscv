
#' @title Plot partial autocorrelation function
#'
#' @description This function plots the sample partial autocorrelation function of one or more time series.
#'
#' @param data A \code{tsibble} in long format with one measurement variable.
#' @param lag_max Integer value. Maximum number of lags.
#' @param level Numeric value. The confidence level to check significance.
#' @param title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param caption Caption of the plot.
#' @param bar_width Numeric value. The width of the bars.
#' @param bar_color1 Character value. The color of the significant bars.
#' @param bar_color2 Character value. The color of the non-significant bars.
#' @param bar_alpha Numeric value. The transparency of the bars.
#' @param line_width Numeric value. Line width of the confidence line.
#' @param line_type Character value. Line type of the confidence line.
#' @param line_color Character value. Line color of the confidence line.
#' @param theme_set A complete ggplot2 theme.
#' @param theme_config A list with further arguments passed to \code{ggplot2::theme()}.
#'
#' @return p An object of class ggplot.
#' @export

plot_pacf <- function(data,
                      lag_max = 25,
                      level = 0.9,
                      title = NULL,
                      subtitle = NULL,
                      xlab = NULL,
                      ylab = NULL,
                      caption = NULL,
                      bar_width = 1,
                      bar_color1 = "#00BFC4",
                      bar_color2 = "#F8766D",
                      bar_alpha = 1,
                      line_width = 0.25,
                      line_type = "solid",
                      line_color = "grey35",
                      theme_set = theme_tscv(),
                      theme_config = list()) {

  dttm <- index_var(data)
  target <- target_vars(data)
  value <- value_var(data)

  # Calculate confidence limits
  sign_tbl <- data %>%
    as_tibble() %>%
    select(-!!sym(dttm)) %>%
    group_by(!!!syms(target)) %>%
    summarise(n_obs = n()) %>%
    ungroup() %>%
    mutate(conf = qnorm((1 - level) / 2) / sqrt(.data$n_obs))

  # Estimate sample autocorrelation function
  data <- data %>%
    PACF(!!sym(value), lag_max = lag_max) %>%
    as_tibble() %>%
    rename(PACF = pacf) %>%
    select(!!!syms(target), PACF) %>%
    gather(
      key = "type",
      value = "value",
      -c(!!!syms(target)))

  # Prepare data for plotting
  data <- left_join(
    data,
    sign_tbl,
    by = target) %>%
    group_by(!!!syms(target)) %>%
    mutate(lag = row_number()) %>%
    mutate(sign = ifelse(abs(value) > abs(.data$conf), TRUE, FALSE)) %>%
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
  p <- p + facet_wrap(
    vars(!!!syms(target)),
    scales = "free")

  # Color bars depending on significance
  if (all(data$sign == FALSE)) {
    p <- p + scale_fill_manual(values = c(bar_color2))
  } else if (all(data$sign == TRUE)) {
    p <- p + scale_fill_manual(values = c(bar_color1))
  } else {
    p <- p + scale_fill_manual(values = c(bar_color2, bar_color1))
  }

  # Scale x axis with integers
  p <- p + scale_x_continuous(breaks = pretty_breaks())

  # Adjust annotations
  p <- p + labs(title = title)
  p <- p + labs(subtitle = subtitle)
  p <- p + labs(x = if_else(is_empty(xlab), "Lag k", xlab))
  p <- p + labs(y = if_else(is_empty(ylab), "PACF", ylab))
  p <- p + labs(caption = caption)

  # Adjust ggplot2 theme
  p <- p + eval(theme_set)
  p <- p + do.call(theme, theme_config)

  return(p)
}
