
#' @title Plot autocorrelation and partial autocorrelation function.
#'
#' @description This function plots the sample autocorrelation and partial autocorrelation function.
#'
#' @param data A valid tsibble in long format with one measurement variable.
#' @param lag_max Integer value. Maximum number of lags.
#' @param demean Logical value. If \code{TRUE}, the time series is demeaned.
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
#' @param theme_set A complete ggplot2 theme.
#' @param theme_config A list with further arguments passed to \code{ggplot2::theme()}.
#'
#' @return p An object of class ggplot.
#' @export

plot_corr <- function(data,
                      lag_max = 48,
                      demean = TRUE,
                      level = 0.9,
                      title = NULL,
                      subtitle = NULL,
                      xlab = NULL,
                      ylab = NULL,
                      caption = NULL,
                      bar_width = 1,
                      bar_color = "#31688EFF",
                      bar_color2 = "#D55E00",
                      bar_alpha = 0.6,
                      line_width = 0.25,
                      line_type = "solid",
                      line_color = "grey35",
                      theme_set = theme_tscv(),
                      theme_config = list()) {

  date_time <- index_var(data)
  variable <- key_vars(data)
  value <- measured_vars(data)

  # Calculate confidence limits
  sign_tbl <- data %>%
    as_tibble() %>%
    select(-!!sym(date_time)) %>%
    group_by(!!!syms(variable)) %>%
    summarise(n_obs = n()) %>%
    ungroup() %>%
    mutate(conf = qnorm((1 - level) / 2) / sqrt(n_obs))

  # Estimate sample autocorrelation function
  acf <- data %>%
    ACF(!!sym(value), lag_max = lag_max, demean = demean) %>%
    as_tibble() %>%
    rename(ACF = acf) %>%
    select(!!!syms(variable), ACF) %>%
    gather(
      key = "type",
      value = "value",
      -c(!!!syms(variable)))

  # Estimate sample autocorrelation function
  pacf <- data %>%
    PACF(!!sym(value), lag_max = lag_max) %>%
    as_tibble() %>%
    rename(PACF = pacf) %>%
    select(!!!syms(variable), PACF) %>%
    gather(
      key = "type",
      value = "value",
      -c(!!!syms(variable)))

  data <- bind_rows(acf, pacf)

  # Prepare data for plotting
  data <- left_join(
    data,
    sign_tbl,
    by = variable) %>%
    group_by(!!!syms(variable)) %>%
    mutate(lag = row_number()) %>%
    mutate(sign = ifelse(abs(value) > abs(conf), TRUE, FALSE)) %>%
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

  # Faceting by .variable and .slice
  p <- p + facet_grid(
    vars(!!!syms(variable)),
    vars(type),
    scales = "free")

  # Color bars depending on significance
  if (all(data$sign == FALSE)) {
    p <- p + scale_fill_manual(values = c(bar_color2))
  } else if (all(data$sign == TRUE)) {
    p <- p + scale_fill_manual(values = c(bar_color))
  } else {
    p <- p + scale_fill_manual(values = c(bar_color2, bar_color))
  }

  # # Lower confidence interval
  # p <- p + geom_hline(
  #   yintercept = -ci_line,
  #   color = line_color,
  #   size = line_width,
  #   linetype = line_type)
  #
  # # Upper confidence interval
  # p <- p + geom_hline(
  #   yintercept = ci_line,
  #   color = line_color,
  #   size = line_width,
  #   linetype = line_type)

  # Adjust annotations
  p <- p + labs(title = title)
  p <- p + labs(subtitle = subtitle)
  p <- p + labs(x = if_else(is_empty(xlab), "Lag k", xlab))
  p <- p + labs(y = ylab)
  p <- p + labs(caption = caption)

  # Adjust ggplot2 theme
  p <- p + eval(theme_set)
  p <- p + do.call(theme, theme_config)

  return(p)
}
