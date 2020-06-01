
#' @title Seasonal plot of a time series.
#'
#' @description Seasonal plot of one or more time series.
#'
#' @param data A tsibble containing the columns time, variable and value.
#' @param detrend Logical value. If \code{TRUE}, the time series are detrended via STL decomposition.
#' @param title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param caption Caption of the plot.
#' @param line_width Numeric value defining the line width (time series).
#' @param line_type Numeric value defining the line type (time series).
#' @param line_color Character value defining the line color (time series).
#' @param line_alpha Numeric value defining the transparency of the line.
#' @param theme_set A complete ggplot2 theme.
#' @param theme_config A list with further arguments passed to \code{ggplot2::theme()}.
#'
#' @return p An object of class ggplot.
#' @export

plot_season2 <- function(data,
                         detrend = TRUE,
                         title = NULL,
                         subtitle = NULL,
                         xlab = NULL,
                         ylab = NULL,
                         caption = NULL,
                         line_width = 0.75,
                         line_type = "solid",
                         line_color = "grey35",
                         line_alpha = 1,
                         theme_set = theme_tscv(),
                         theme_config = list()) {

  date_time <- index_var(data)
  variable <- key_vars(data)
  value <- measured_vars(data)

  if (detrend == TRUE) {
    data <- data %>%
      model(STL(!!sym(value))) %>%
      components() %>%
      mutate(!!sym(value) := !!sym(value) - trend) %>%
      select(
        !!!syms(variable),
        !!sym(date_time),
        !!sym(value))
  }

  # Check frequency for grouping
  freq <- data %>%
    pull(!!sym(date_time)) %>%
    guess_frequency()

  if (freq == 4) {freq <- "qtr"}
  if (freq == 12) {freq <- "month"}

  data <- data %>%
    as_tibble() %>%
    mutate(year = year(!!sym(date_time))) %>%
    mutate(qtr = as_factor(paste0("Q", quarter(!!sym(date_time))))) %>%
    mutate(month = as_factor(month(!!sym(date_time), label = TRUE))) %>%
    select(-!!sym(date_time)) %>%
    group_by(!!!syms(variable), !!sym(freq)) %>%
    summarise(
      `10%` = quantile(!!sym(value), probs = 0.10, na.rm = TRUE),
      `25%` = quantile(!!sym(value), probs = 0.25, na.rm = TRUE),
      median = quantile(!!sym(value), probs = 0.5, na.rm = TRUE),
      `75%` = quantile(!!sym(value), probs = 0.75, na.rm = TRUE),
      `90%` = quantile(!!sym(value), probs = 0.90, na.rm = TRUE)) %>%
    ungroup()


  # Create ggplot
  p <- ggplot(data = data)

  p <- p + facet_grid(
    rows = vars(!!!syms(variable)),
    scales = "free")

  p <- p + geom_ribbon(
    aes(
      x = !!sym(freq),
      ymin = `10%`,
      ymax = `90%`,
      group = "10%-90%",
      fill = "10%-90%"),
    alpha = 0.2)

  p <- p + geom_ribbon(
    aes(
      x = !!sym(freq),
      ymin = `25%`,
      ymax = `75%`,
      group = "25%-75%",
      fill = "25%-75%"),
    alpha = 0.3)

  p <- p + scale_fill_manual(
    values = c("#31688EFF", "#31688EFF"))

  p <- p + geom_line(
    aes(
      x = !!sym(freq),
      y = median,
      group = "Median",
      color = "Median"),
    size = line_width,
    linetype = line_type,
    alpha = line_alpha)

  p <- p + scale_color_manual(
    values = line_color)

  # Adjust annotations
  p <- p + labs(title = title)
  p <- p + labs(subtitle = subtitle)
  p <- p + labs(x = if_else(is_empty(xlab), date_time, xlab))
  p <- p + labs(y = if_else(is_empty(ylab), value, ylab))
  p <- p + labs(caption = caption)

  # Adjust ggplot2 theme
  p <- p + eval(theme_set)
  p <- p + do.call(theme, theme_config)

  return(p)
}
