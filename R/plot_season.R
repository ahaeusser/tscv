#' @title Seasonal plot of a time series.
#'
#' @description Seasonal plot of one or more time series.
#'
#' @param data A tsibble containing the columns time, variable and value.
#' @param title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param caption Caption of the plot.
#' @param line_width Numeric value defining the line width (time series).
#' @param line_type Numeric value defining the line type (time series).
#' @param line_color Character value defining the line color (time series).
#' @param line_alpha Numeric value defining the transparency of the line.
#'
#' @return p An object of class ggplot.
#' @export

plot_season <- function(data,
                        title = NULL,
                        subtitle = NULL,
                        xlab = "Hour",
                        ylab = NULL,
                        caption = NULL,
                        line_width = 0.75,
                        line_type = "solid",
                        line_color = "#31688EFF",
                        line_alpha = 1) {

  # Prepare data
  data <- data %>%
    mutate(wday = lubridate::wday(date_time, label = TRUE, week_start = getOption("lubridate.week.start", 1))) %>%
    mutate(hour = lubridate::hour(date_time)) %>%
    as_tibble() %>%
    select(-date_time) %>%
    group_by(variable, wday, hour) %>%
    summarise(
      `10%` = quantile(value, probs = 0.10),
      `25%` = quantile(value, probs = 0.25),
      median = quantile(value, probs = 0.5),
      `75%` = quantile(value, probs = 0.75),
      `90%` = quantile(value, probs = 0.90)) %>%
    ungroup()


  # Create ggplot
  p <- ggplot2::ggplot(data = data)

  p <- p + facet_grid(
    cols = vars(wday),
    rows = vars(variable),
    scales = "free")

  p <- p + geom_ribbon(
    aes(
      x = hour,
      ymin = `10%`,
      ymax = `90%`,
      fill = "10%-90%"),
    alpha = 0.2)

  p <- p + geom_ribbon(
    aes(
      x = hour,
      ymin = `25%`,
      ymax = `75%`,
      fill = "25%-75%"),
    alpha = 0.3)

  p <- p + scale_fill_manual(
    values = c("#31688EFF", "#31688EFF"))

  p <- p + geom_line(
    aes(
      x = hour,
      y = median,
      group = wday,
      color = "Median"),
    size = line_width,
    linetype = line_type,
    alpha = line_alpha)

  p <- p + scale_color_manual(
    values = line_color)

  p <- p + scale_y_continuous()
  p <- p + scale_x_continuous(breaks = c(0, 6, 12, 18))

  # Adjust annotations
  p <- p + ggplot2::labs(title = title)
  p <- p + ggplot2::labs(subtitle = subtitle)
  p <- p + ggplot2::labs(x = xlab)
  p <- p + ggplot2::labs(y = ylab)
  p <- p + ggplot2::labs(caption = caption)

  p <- p + theme_tscv()
  return(p)
}
