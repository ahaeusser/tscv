
#' @title Plot forecast accuracy metrics
#'
#' @description Plot forecast accuracy metrics, either along the forecast horizon or the slices. The user can define
#'    the forecasting models and accuracy metrics.
#'
#' @param data A \code{tibble} containing the accuracy metrics, i.e. the result of a call to \code{error_metrics()}.
#' @param model Character vector defining the forecasting models.
#' @param metric Character vector defining the accuracy measures.
#' @param title Title for the plot.
#' @param subtitle Subtitle for the plot.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param caption Caption (footnote) for the plot.
#' @param line_width Numeric value. Line width.
#' @param line_type Character value defining the line type.
#' @param point_size Numeric value. Point size.
#' @param point_shape Integer value. Point shape.
#' @param point_alpha Numeric value between 0 and 1. The transparency of the points.
#' @param theme_set A complete ggplot2 theme.
#' @param theme_config A list with further arguments passed to \code{ggplot2::theme()}.
#' @param ... Further arguments passed to \code{ggplot2::geom_line()} or \code{ggplot2::geom_point()}.
#'
#' @return p An object of class ggplot.
#' @export

plot_error_metrics <- function(data,
                               model = NULL,
                               metric = "MASE",
                               title = NULL,
                               subtitle = NULL,
                               ylab = NULL,
                               xlab = NULL,
                               caption = NULL,
                               line_width = 1,
                               line_type = "solid",
                               point_size = 0,
                               point_shape = 19,
                               point_alpha = 1,
                               theme_set = theme_tscv(),
                               theme_config = list(),
                               ...) {

  data_cols <- names(data)
  def_cols <- c(".model", "horizon", "split", "metric", "value")
  target <- setdiff(data_cols, def_cols)

  if ("split" %in% data_cols) {
    by <- "split"
  } else {
    by <- "horizon"
  }

  # Check arguments
  if (is_empty(model)) {
    set_model <- data %>%
      pull(.data$.model) %>%
      unique()
  } else {
    set_model <- model
  }

  if (is_empty(metric)) {
    set_metric <- data %>%
      pull(metric) %>%
      unique()
  } else {
    set_metric <- metric
  }

  data <- data %>%
    filter(.data$metric %in% set_metric) %>%
    filter(.data$.model %in% set_model)

  # Initialize plot
  p <- ggplot(
    data = data,
    aes(
      x = !!sym(by),
      y = .data$value,
      colour = .data$.model,
      group = .data$.model))

  # Add lines and point
  p <- p + geom_line(
    na.rm = TRUE,
    size = line_width,
    linetype = line_type,
    ...)

  p <- p + geom_point(
    na.rm = TRUE,
    size = point_size,
    shape = point_shape,
    alpha = point_alpha,
    ...)

  # Scale axis and aesthetics
  p <- p + scale_y_continuous()

  # Create faceting (by target variables and metric)
  p <- p + facet_grid(
    vars(.data$metric),
    vars(!!!syms(target)),
    scales = "free")

  # Scale x axis with integers
  p <- p + scale_x_continuous(breaks = pretty_breaks())

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
