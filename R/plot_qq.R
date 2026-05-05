
#' @title Create a quantile-quantile plot
#'
#' @description
#' Create a quantile-quantile plot for one or more numeric variables.
#'
#' @details
#' \code{plot_qq()} is a convenience wrapper around the \code{qqplotr}
#' functions \code{stat_qq_point()}, \code{stat_qq_line()}, and
#' \code{stat_qq_band()}. It is useful for checking whether values, residuals,
#' or forecast errors approximately follow a theoretical distribution.
#'
#' By default, the function creates a normal quantile-quantile plot with
#' pointwise confidence bands.
#'
#' The arguments \code{x}, \code{facet_var}, and \code{color} are passed as
#' unquoted column names.
#'
#' If \code{color} is supplied, point colors, line colors, and confidence-band
#' fills are mapped to that variable. In this case, \code{point_color},
#' \code{point_fill}, \code{line_color}, and \code{band_color} are ignored.
#' If \code{color} is not supplied, the fixed styling arguments are used.
#'
#' Additional arguments can be passed to the underlying \code{qqplotr}
#' statistics through \code{...}, for example distributional arguments supported
#' by \code{qqplotr}.
#'
#' Additional theme settings can be supplied through \code{theme_config}. This
#' should be a named list of arguments passed to \code{ggplot2::theme()}.
#'
#' @param data A \code{data.frame}, \code{tibble}, or \code{tsibble} in long
#'   format.
#' @param x Unquoted column in \code{data} containing numeric values.
#' @param facet_var Optional unquoted column in \code{data} used for faceting.
#' @param facet_scale Character value defining facet axis scaling. Common values
#'   are \code{"free"}, \code{"fixed"}, \code{"free_x"}, and \code{"free_y"}.
#' @param facet_nrow Optional integer. Number of rows in the facet layout.
#' @param facet_ncol Optional integer. Number of columns in the facet layout.
#' @param color Optional unquoted column in \code{data} used to map point, line,
#'   and confidence-band colors.
#' @param title Character value. Plot title.
#' @param subtitle Character value. Plot subtitle.
#' @param xlab Character value. Label for the x-axis.
#' @param ylab Character value. Label for the y-axis.
#' @param caption Character value. Plot caption.
#' @param point_size Numeric value defining the point size.
#' @param point_shape Numeric or character value defining the point shape.
#' @param point_color Character value defining the point outline color. Ignored
#'   when \code{color} is supplied.
#' @param point_fill Character value defining the point fill color. Ignored when
#'   \code{color} is supplied.
#' @param point_alpha Numeric value between \code{0} and \code{1} defining point
#'   transparency.
#' @param line_width Numeric value defining the qq-line width.
#' @param line_type Character or numeric value defining the qq-line type.
#' @param line_color Character value defining the qq-line color. Ignored when
#'   \code{color} is supplied.
#' @param line_alpha Numeric value between \code{0} and \code{1} defining line
#'   transparency.
#' @param band_color Character value defining the confidence-band fill color.
#'   Ignored when \code{color} is supplied.
#' @param band_alpha Numeric value between \code{0} and \code{1} defining
#'   confidence-band transparency.
#' @param theme_set A complete \code{ggplot2} theme.
#' @param theme_config A named \code{list} with additional arguments passed to
#'   \code{ggplot2::theme()}.
#' @param ... Further arguments passed to \code{qqplotr::stat_qq_point()},
#'   \code{qqplotr::stat_qq_line()}, and \code{qqplotr::stat_qq_band()}.
#'
#' @return
#' An object of class \code{ggplot}.
#'
#' @family data visualization
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' data <- M4_monthly_data |>
#'   filter(series %in% c("M23100", "M14395"))
#'
#' plot_qq(
#'   data = data,
#'   x = value,
#'   facet_var = series,
#'   title = "QQ Plot of M4 Monthly Values",
#'   subtitle = "Normal quantile-quantile plots by series",
#'   xlab = "Theoretical quantiles",
#'   ylab = "Sample quantiles"
#' )
#'
#' stats <- data |>
#'   group_by(series) |>
#'   mutate(value_centered = value - mean(value, na.rm = TRUE)) |>
#'   ungroup()
#'
#' plot_qq(
#'   data = stats,
#'   x = value_centered,
#'   color = series,
#'   title = "QQ Plot of Centered M4 Monthly Values",
#'   subtitle = "Normal quantile-quantile plots by series",
#'   xlab = "Theoretical quantiles",
#'   ylab = "Sample quantiles"
#' )

plot_qq <- function(data,
                    x,
                    facet_var = NULL,
                    facet_scale = "free",
                    facet_nrow = NULL,
                    facet_ncol = NULL,
                    color = NULL,
                    title = NULL,
                    subtitle = NULL,
                    xlab = NULL,
                    ylab = NULL,
                    caption = NULL,
                    point_size = 2,
                    point_shape = 16,
                    point_color = "grey35",
                    point_fill = "grey35",
                    point_alpha = 0.25,
                    line_width = 0.25,
                    line_type = "solid",
                    line_color = "grey35",
                    line_alpha = 1,
                    band_color = "grey35",
                    band_alpha = 0.25,
                    theme_set = theme_tscv(),
                    theme_config = list(),
                    ...) {

  # Create initial ggplot object
  p <- ggplot(data = data)

  # Create QQ plot
  if (quo_is_null(enquo(color))) {
    # Create points
    p <- p + stat_qq_point(
      aes(sample = {{x}}),
      color = point_color,
      fill = point_fill,
      alpha = point_alpha,
      size = point_size,
      shape = point_shape,
      ...
    )

    # Create line
    p <- p + stat_qq_line(
      aes(sample = {{x}}),
      color = line_color,
      linewidth = line_width,
      linetype = line_type,
      alpha = line_alpha,
      ...
    )

    # Create confidence bands
    p <- p + stat_qq_band(
      aes(sample = {{x}}),
      bandType = "pointwise",
      fill = band_color,
      alpha = band_alpha,
      ...
    )
  } else {
    # Create points
    p <- p + stat_qq_point(
      aes(
        sample = {{x}},
        color = {{color}},
        fill = {{color}}
      ),
      alpha = point_alpha,
      size = point_size,
      shape = point_shape,
      ...
    )

    # Create line
    p <- p + stat_qq_line(
      aes(
        sample = {{x}},
        color = {{color}}
      ),
      linewidth = line_width,
      linetype = line_type,
      alpha = line_alpha,
      ...
    )

    # Create confidence bands
    p <- p + stat_qq_band(
      aes(
        sample = {{x}},
        fill = {{color}}
      ),
      bandType = "pointwise",
      alpha = band_alpha,
      ...
    )
  }

  # Create facet
  if (!quo_is_null(enquo(facet_var))) {
    p <- p + facet_wrap(
      facets = vars({{facet_var}}),
      scales = facet_scale,
      nrow = facet_nrow,
      ncol = facet_ncol
    )
  }

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
