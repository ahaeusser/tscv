
#' @title Plot forecast accuracy metrics.
#'
#' @description Plot forecast accuracy metrics, either along the forecast horizon or the slices. The user can define
#'    specific target variable(s), forecasting method(s) and accuracy metric(s).
#'
#' @param data A tibble containing the accuracy metrics, i.e. the result of a call to \code{error_metrics(...)}.
#' @param variable Character vector defining the target variable.
#' @param metric Character vector defining the accuracy measures.
#' @param model Character vector defining the forecasting methods.
#' @param title Title for the plot.
#' @param subtitle Subtitle for the plot.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param caption Caption (footnote) for the plot.
#' @param line_width Numeric value. Line width.
#' @param point_size Numeric value. Point size.
#' @param point_shape Integer value. Point shape.
#' @param point_alpha Numeric value between 0 and 1. The transparency of the points.
#' @param base_size Integer value. Base font size.
#'
#' @return p An object of class ggplot.
#'
#' @export

plot_error_metrics <- function(data,
                               variable,
                               model,
                               metric,
                               dim = "horizon",
                               title = "Evaluation of forecast accuracy",
                               ylab = NULL,
                               caption = NULL,
                               line_width = 1.5,
                               point_size = 3,
                               point_shape = 19,
                               point_alpha = 1,
                               base_size = 11) {
  # Check arguments
  select_variable <- variable
  select_model <- model
  select_metric <- metric
  select_dim <- dim

  if (length(select_dim) > 1) {
    stop("Please select only one dimension, either 'horizon' or 'slice'.")
  }

  # Preparation
  if (select_dim == "horizon") {
    xlab <- "Forecast horizon (n-step)"
  } else if (select_dim == "slice") {
    xlab <- "Time slice"
  }

  subtitle <- paste("Forecast accuracy by ", select_dim, sep = "")

  data_plot <- data %>%
    filter(variable %in% select_variable) %>%
    filter(metric %in% select_metric) %>%
    filter(model %in% select_model) %>%
    filter(dim %in% select_dim)

  # Initialize plot
  p <- ggplot(
    data = data_plot,
    aes(
      x = num,
      # x = factor(num),
      y = value,
      colour = model,
      group = model))

  # Add lines and point
  p <- p + geom_line(
    na.rm = TRUE,
    size = line_width)

  # p <- p + geom_point(
  #   na.rm = TRUE,
  #   size = point_size,
  #   alpha = point_alpha)

  # Scale axis and aesthetics
  p <- p + scale_y_continuous()
  #p <- p + scale_x_discrete()
  p <- p + scale_color_viridis_d()

  # Create faceting
  p <- p + facet_wrap(
    ~variable + ~metric,
    scales = "free")

  # Adjust annotations
  p <- p + labs(title = title)
  p <- p + labs(subtitle = subtitle)
  p <- p + labs(x = xlab)
  p <- p + labs(y = ylab)
  p <- p + labs(caption = caption)
  # Adjust theme
  p <- p + theme_tscv(base_size = base_size)
  return(p)
}
