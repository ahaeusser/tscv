
#' @title Plot the forecast errors along the forecast horizon.
#'
#' @description Plot the forecast errors along the forecast horizon (and corresponding Box-Whisker Plots) for a specific target variable and forecasting methods.
#'
#' @param data A tsibble containing the forecasts errors for all target variables, slice, models etc. provided by the function \code{errors()}.
#' @param model Character vector. Choose the forecasting models.
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
#' @param box_line_color Character value. The outline color of the Box-Whisker.
#' @param box_fill_alpha Numeric value. The transparency of the Box-Whisker.
#' @param theme_set A complete ggplot2 theme.
#' @param theme_config A list with further arguments passed to \code{ggplot2::theme()}.
#'
#' @return p An object of class ggplot
#' @export

plot_error_dist <- function(data,
                            model = NULL,
                            add_points = FALSE,
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
                            box_line_color = "black",
                            box_fill_alpha = 0.75,
                            theme_set = theme_tscv(),
                            theme_config = list()) {

  response <- response_vars(data)

  if (is.null(model)) {
    select_model <- data %>%
      pull(.model) %>%
      unique()
  } else {
    select_model <- model
  }

  # Prepare data
  data <- data %>%
    filter(.model %in% select_model)

  # Visualize forecast errors and distribution as Box-Whisker
  p <- ggplot(
    data,
    aes(
      x = horizon,
      y = error,
      fill = .model,
      group = horizon
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
    color = box_line_color,
    alpha = box_fill_alpha)

  # Scale axis and aesthetics
  p <- p + scale_y_continuous()

  p <- p + facet_grid(
    vars(!!!syms(response)),
    vars(.model),
    scales = "free")

  # Adjust annotations
  p <- p + labs(title = title)
  p <- p + labs(subtitle = subtitle)
  p <- p + labs(x = xlab)
  p <- p + labs(y = ylab)
  p <- p + labs(caption = caption)

  # Adjust ggplot2 theme
  p <- p + eval(theme_set)
  p <- p + do.call(theme, theme_config)
  return(p)
}
