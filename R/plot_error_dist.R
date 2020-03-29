#' @title Plot the forecast errors along the forecast horizon.
#'
#' @description Plot the forecast errors along the forecast horizon (and corresponding Box-Whisker Plots) for a specific target variable and forecasting methods.
#'
#' @param data A tsibble containing the forecasts errors for all target variables, slice, models etc. provided by the function \code{create_errors(...)}.
#' @param variable Character value. The target variable.
#' @param model Character vector. Choose the forecasting methods (e.g. c("ARIMA", "ETS"))
#' @param add_points Logical value. If \code{TRUE}, the errors are plotted as points. Ohterwise, Box-Whisker plots only.
#' @param jitter Logical value. If \code{TRUE}, points are jittered.
#' @param title Title for the plot
#' @param subtitle Subtitle for the plot
#' @param xlab Label for the x-axis
#' @param ylab Label for the y-axis
#' @param caption Caption of the plot.
#' @param point_size Numeric value defining the point size.
#' @param pointshape Integer value defining the point shape.
#' @param point_alpha Numeric value. The transparency of the points.
#' @param box_line_width Numeric value defining the line width of the Box-Whisker.
#' @param box_alpha Numeric value. The transparency of the Box-Whisker.
#' @param base_size Integer value. Base font size.
#'
#' @return p An object of class ggplot
#' @export

plot_error_dist <- function(data,
                            variable = NULL,
                            model = NULL,
                            add_points = TRUE,
                            jitter = TRUE,
                            title = "Evaluation of forecast accuracy",
                            subtitle = "Distribution of forecast errors by horizon",
                            xlab = "Forecast horizon (n-step)",
                            ylab = "Forecast error",
                            caption = NULL,
                            point_size = 2,
                            point_shape = 19,
                            point_alpha = 0.25,
                            box_line_width = 0.25,
                            box_alpha = 0.75,
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
      pull(model) %>%
      unique()
  } else {
    select_model <- model
  }

  # Prepare data ................................................................

  # Filter by variable and slice
  data_plot <- data %>%
    filter(variable %in% select_variable) %>%
    filter(model %in% select_model) %>%
    filter(type == "error")

  # Visualize forecast errors and distributions .................................
  p <- ggplot(
    data_plot,
    aes(
      x = horizon,
      y = value,
      fill = model,
      group = horizon,
      colour = model
    )
  )

  # Create points
  if (add_points == TRUE) {
    if (jitter == TRUE) {
      p <- p + geom_point(
        size = point_size,
        shape = point_shape,
        position = position_jitter(
          width = 0.2,
          height = 0.1
        ),
        alpha = point_alpha,
        na.rm = TRUE
      )
    } else {
      p <- p + geom_point(
        size = point_size,
        shape = point_shape,
        alpha = point_alpha,
        na.rm = TRUE
      )
    }
  }

  # Create Box-Whisker-Plots
  p <- p + geom_boxplot(
    na.rm = TRUE,
    outlier.shape = NA,
    lwd = box_line_width,
    alpha = box_alpha)

  # Scale axis and aesthetics
  p <- p + scale_y_continuous()

  p <- p + facet_grid(
    vars(variable),
    vars(model),
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
