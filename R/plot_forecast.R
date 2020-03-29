#' @title Plot the actual and fitted values and forecasts.
#'
#' @description Plot the actual and fitted values and forecasts for specific target variable(s), slice(s) and forecasting method(s).
#'
#' @param data A tsibble according to the adice data model.
#' @param variable Character vector defining the variable(s).
#' @param slice Integer vector. The time slice of the rolling forecast.
#' @param model Character vector. The forecasting methods.
#' @param include Integer value. The number of actual values (training) to be included.
#' @param title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param caption Caption of the plot.
#' @param line_width Numeric value defining the line width.
#' @param point_size Numeric value defining the point size.
#' @param point_alpha Numeric value. The transparency of the points.
#' @param base_size Integer value. Base font size.
#'
#' @return p An object of class ggplot.
#' @export

plot_forecast <- function(data,
                          variable = NULL,
                          slice = NULL,
                          model = NULL,
                          include = 24,
                          title = NULL,
                          subtitle = NULL,
                          xlab = "Date",
                          ylab = NULL,
                          caption = NULL,
                          line_width = 1.5,
                          point_size = 2,
                          point_alpha = 0,
                          base_size = 11) {

  # Check arguments
  if (is.null(variable)) {
    select_variable <- data %>%
      pull(variable) %>%
      unique()
  } else {
    select_variable <- variable
  }

  if (is.null(model)) {
    select_model <- data %>%
      filter(type == "fcst") %>%
      pull(model) %>%
      unique()
  } else {
    select_model <- model
  }

  if (is.null(slice)) {
    select_slice <- 1
  } else {
    select_slice <- slice
  }

  # Prepare data ..............................................................

  # Filter by variable and slice
  data_plot <- data %>%
    filter(variable %in% select_variable) %>%
    filter(slice %in% select_slice)

  # Prepare forecasts for specified methods
  fcst <- data_plot %>%
    filter(type == "fcst") %>%
    filter(model %in% select_model)

  # Prepare actual and fitted values
  actual <- data_plot %>%
    filter(type == "actual")

  # Cut actual and fitted values to correct length
  if (!is.null(include)) {
    n_ahead <- actual %>%
      filter(sample == "test") %>%
      pull(horizon) %>%
      max(na.rm = TRUE)

    actual <- actual %>%
      group_by(variable, slice) %>%
      slice((n() - include - n_ahead + 1):n()) %>%
      ungroup()
  }

  # Visualize actual and fitted values and forecasts ..........................

  # Initialize plot
  p <- ggplot()
  # Create line for actual values
  p <- p + geom_line(
    data = actual,
    aes(x = date_time,
        y = value),
    na.rm = TRUE,
    size = 0.5,
    color = "black")

  # Create line for forecasts
  p <- p + geom_line(
    data = fcst,
    aes(x = date_time,
        y = value,
        color = model),
    na.rm = TRUE,
    size = line_width)

  # Faceting by .variable and .slice
  p <- p + facet_grid(
    vars(variable),
    vars(slice),
    scales = "free")

  # Create points for forecasts
  p <- p + geom_point(
    data = fcst,
    aes(x = date_time,
        y = value,
        color = model),
    na.rm = TRUE,
    size = point_size,
    alpha = point_alpha)

  # Adjust annotations and theme
  p <- p + labs(title = title)
  p <- p + labs(subtitle = subtitle)
  p <- p + labs(x = xlab)
  p <- p + labs(y = ylab)
  p <- p + labs(caption = caption)
  p <- p + theme_tscv(base_size = base_size)
  return(p)
}
