
#' @title Plot the actual and fitted values and forecasts.
#'
#' @description Plot the actual and fitted values and forecasts for specific target variable(s), split(s) and forecasting method(s).
#'
#' @param fcst A `fable` containing the forecasts for the models, splits, etc.
#' @param data A `tsibble` containing the training and testing data.
#' @param split Integer vector. The split id (time slices) of the time series cross-validation.
#' @param model Character vector. The forecasting model(s).
#' @param include Integer value. The number of actual values (training) to be included.
#' @param title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param caption Caption of the plot.
#' @param line_width Numeric value defining the line width.
#' @param point_size Numeric value defining the point size.
#' @param point_alpha Numeric value. The transparency of the points.
#' @param theme_set A complete ggplot2 theme.
#' @param theme_config A list with further arguments passed to \code{ggplot2::theme()}.
#'
#' @return p An object of class ggplot.
#' @export

plot_forecast <- function(fcst,
                          data,
                          split = NULL,
                          model = NULL,
                          include = 24,
                          title = NULL,
                          subtitle = NULL,
                          xlab = NULL,
                          ylab = NULL,
                          caption = NULL,
                          line_width = 1,
                          point_size = 2,
                          point_alpha = 0,
                          theme_set = theme_tscv(),
                          theme_config = list()) {

  # Check arguments
  if (is_empty(model)) {
    select_model <- fcst %>%
      pull(.model) %>%
      unique()
  } else {
    select_model <- model
  }

  if (is_empty(split)) {
    select_split <- 1
  } else {
    select_split <- split
  }

  dttm <- index_var(fcst)
  response <- response_vars(fcst)
  value <- value_var(fcst)

  # Prepare forecasts and actual values for specified slices and models
  fcst <- fcst %>%
    filter(split %in% select_split) %>%
    filter(.model %in% select_model)

  actual <- data %>%
    filter(split %in% select_split)

  # Cut actual and fitted values to correct length
  if (!is_empty(include)) {
    n_ahead <- actual %>%
      filter(sample == "test") %>%
      pull(horizon) %>%
      max(na.rm = TRUE)

    actual <- actual %>%
      group_by(!!!syms(response), split) %>%
      slice((n() - include - n_ahead + 1):n()) %>%
      ungroup()
  }

  # Visualize actual values and forecasts
  p <- ggplot()
  # Create line for actual values
  p <- p + geom_line(
    data = actual,
    aes(
      x = !!sym(dttm),
      y = !!sym(value)),
    na.rm = TRUE,
    size = 0.5,
    color = "black")

  # Create line for forecasts
  p <- p + geom_line(
    data = fcst,
    aes(
      x = !!sym(dttm),
      y = !!sym(value),
      color = .model),
    na.rm = TRUE,
    size = line_width)

  # Faceting by keys and split
  p <- p + facet_grid(
    vars(!!!syms(response)),
    vars(split),
    scales = "free")

  # Create points for forecasts
  p <- p + geom_point(
    data = fcst,
    aes(
      x = !!sym(dttm),
      y = !!sym(value),
      color = model),
    na.rm = TRUE,
    size = point_size,
    alpha = point_alpha)

  # Adjust annotations and theme
  p <- p + labs(title = title)
  p <- p + labs(subtitle = subtitle)
  p <- p + labs(x = if_else(is_empty(xlab), dttm, xlab))
  p <- p + labs(y = ylab)
  p <- p + labs(caption = caption)

  # Adjust ggplot2 theme
  p <- p + eval(theme_set)
  p <- p + do.call(theme, theme_config)
  return(p)
}
