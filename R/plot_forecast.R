
#' @title Plot the actual and fitted values and forecasts.
#'
#' @description Plot the actual and fitted values and forecasts for specific target variable(s), slice(s) and forecasting method(s).
#'
#' @param data A tsibble containing the data and the forecasts.
#' @param slice Integer vector. The time slice(s) of the time series cross-validation.
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

plot_forecast <- function(data,
                          slice = NULL,
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
    select_model <- data %>%
      filter(type == "fcst") %>%
      pull(model) %>%
      unique()
  } else {
    select_model <- model
  }

  if (is_empty(slice)) {
    select_slice <- 1
  } else {
    select_slice <- slice
  }

  date_time <- index_var(data)
  all_keys <- key_vars(data)
  all_meas <- measured_vars(data)

  # Exclude 'helper' columns (key and measured variables)
  variable <- all_keys[!all_keys %in% c("slice", "model", "type")]
  value <- all_meas[!all_meas %in% c("sample", "horizon")]

  # Prepare data ..............................................................

  # Prepare forecasts and actual values for specified slices and models

  fcst <- data %>%
    filter(slice %in% select_slice) %>%
    filter(type == "fcst") %>%
    filter(model %in% select_model)

  actual <- data %>%
    filter(slice %in% select_slice) %>%
    filter(type == "actual")

  # Cut actual and fitted values to correct length
  if (!is_empty(include)) {
    n_ahead <- actual %>%
      filter(sample == "test") %>%
      pull(horizon) %>%
      max(na.rm = TRUE)

    actual <- actual %>%
      group_by(!!!syms(variable), slice) %>%
      slice((n() - include - n_ahead + 1):n()) %>%
      ungroup()
  }

  # Visualize actual and fitted values and forecasts ..........................

  # Initialize plot
  p <- ggplot()

  # Create line for actual values
  p <- p + geom_line(
    data = actual,
    aes(x = !!sym(date_time),
        y = !!sym(value)),
    na.rm = TRUE,
    size = 0.5,
    color = "black")

  # Create line for forecasts
  p <- p + geom_line(
    data = fcst,
    aes(x = !!sym(date_time),
        y = !!sym(value),
        color = model),
    na.rm = TRUE,
    size = line_width)

  # Faceting by .variable and .slice
  p <- p + facet_grid(
    vars(!!!syms(variable)),
    vars(slice),
    scales = "free")

  # Create points for forecasts
  p <- p + geom_point(
    data = fcst,
    aes(x = !!sym(date_time),
        y = !!sym(value),
        color = model),
    na.rm = TRUE,
    size = point_size,
    alpha = point_alpha)

  # Adjust annotations and theme
  p <- p + labs(title = title)
  p <- p + labs(subtitle = subtitle)
  p <- p + labs(x = if_else(is_empty(xlab), date_time, xlab))
  p <- p + labs(y = ylab)
  p <- p + labs(caption = caption)

  # Adjust ggplot2 theme
  p <- p + eval(theme_set)
  p <- p + do.call(theme, theme_config)

  return(p)
}
