
#' @title Seasonal plot of a time series.
#'
#' @description Seasonal plot of one or more time series.
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
#' @param line_alpha Numeric value defining the transparency of the line.
#' @param theme_set A complete ggplot2 theme.
#' @param theme_config A list with further arguments passed to \code{ggplot2::theme()}.
#'
#' @return p An object of class ggplot.
#' @export

plot_season <- function(data,
                        title = NULL,
                        subtitle = NULL,
                        xlab = NULL,
                        ylab = NULL,
                        caption = NULL,
                        line_width = 0.75,
                        line_type = "solid",
                        line_color = "#31688E",
                        line_alpha = 1,
                        fill_color = "#31688E",
                        fill_alpha = 0.2,
                        theme_set = theme_tscv(),
                        theme_config = list()) {

  dttm <- index_var(data)
  response <- response_vars(data)
  value <- value_var(data)

  # Prepare data
  data <- data %>%
    mutate(wday = lubridate::wday(!!sym(dttm), label = TRUE, week_start = getOption("lubridate.week.start", 1))) %>%
    mutate(hour = lubridate::hour(!!sym(dttm))) %>%
    as_tibble() %>%
    select(-!!sym(dttm)) %>%
    group_by(!!!syms(response), wday, hour) %>%
    summarise(
      median = median(!!sym(value), na.rm = TRUE),
      mad = mad(!!sym(value), na.rm = TRUE)) %>%
    ungroup()

  # Create ggplot
  p <- ggplot(data = data)

  p <- p + facet_grid(
    cols = vars(wday),
    rows = vars(!!!syms(response)),
    scales = "free")

  p <- p + geom_ribbon(
    aes(
      x = hour,
      ymin = median - mad,
      ymax = median + mad,
      fill = "MAD"),
    alpha = fill_alpha)

  p <- p + scale_fill_manual(
    values = c(fill_color))

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
