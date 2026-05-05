
#' @title Plot a kernel density estimate
#'
#' @description
#' Create a density plot for one or more numeric variables using kernel density
#' estimation.
#'
#' @details
#' \code{plot_density()} is a convenience wrapper around
#' \code{ggplot2::geom_density()}. It is useful for comparing the distribution
#' of values across one or more time series, models, groups, or residual sets.
#'
#' The arguments \code{x}, \code{facet_var}, \code{color}, and \code{fill} are
#' passed as unquoted column names.
#'
#' If \code{color} is supplied, both line color and fill color are mapped to that
#' variable. In this case, \code{line_color} and \code{fill_color} are ignored.
#' If \code{color} is not supplied, all density curves use \code{line_color} and
#' \code{fill_color}.
#'
#' Missing values are removed before plotting.
#'
#' Additional arguments can be passed to \code{ggplot2::geom_density()} through
#' \code{...}, for example \code{adjust}, \code{bw}, or \code{kernel}.
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
#' @param color Optional unquoted column in \code{data} used to map density line
#'   and fill color.
#' @param fill Optional unquoted column in \code{data} used to map density fill
#'   color. Currently not used directly; use \code{color} for grouped density
#'   plots.
#' @param title Character value. Plot title.
#' @param subtitle Character value. Plot subtitle.
#' @param xlab Character value. Label for the x-axis.
#' @param ylab Character value. Label for the y-axis.
#' @param caption Character value. Plot caption.
#' @param line_width Numeric value defining the density line width.
#' @param line_type Character or numeric value defining the density line type.
#' @param line_color Character value defining the density line color. Ignored
#'   when \code{color} is supplied.
#' @param fill_color Character value defining the fill color under the density
#'   curve. Ignored when \code{color} is supplied.
#' @param fill_alpha Numeric value between \code{0} and \code{1} defining fill
#'   transparency.
#' @param theme_set A complete \code{ggplot2} theme.
#' @param theme_config A named \code{list} with additional arguments passed to
#'   \code{ggplot2::theme()}.
#' @param ... Further arguments passed to \code{ggplot2::geom_density()}.
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
#' plot_density(
#'   data = data,
#'   x = value,
#'   facet_var = series,
#'   title = "Distribution of M4 Monthly Values",
#'   subtitle = "Kernel density estimates by series",
#'   xlab = "Value",
#'   ylab = "Density"
#' )
#'
#' plot_density(
#'   data = data,
#'   x = value,
#'   color = series,
#'   title = "Distribution of M4 Monthly Values",
#'   subtitle = "Kernel density estimates by series",
#'   xlab = "Value",
#'   ylab = "Density",
#'   adjust = 1.2
#' )

plot_density <- function(data,
                         x,
                         facet_var = NULL,
                         facet_scale = "free",
                         facet_nrow = NULL,
                         facet_ncol = NULL,
                         color = NULL,
                         fill = NULL,
                         title = NULL,
                         subtitle = NULL,
                         xlab = NULL,
                         ylab = NULL,
                         caption = NULL,
                         line_width = 0.1,
                         line_type = "solid",
                         line_color = "grey35",
                         fill_color = "grey35",
                         fill_alpha = 0.5,
                         theme_set = theme_tscv(),
                         theme_config = list(),
                         ...) {

  # Create initial ggplot object
  p <- ggplot(data = data)

  # Create density plot
  if (quo_is_null(enquo(color))) {
    p <- p + geom_density(
      aes(x = {{x}}),
      na.rm = TRUE,
      color = line_color,
      fill = fill_color,
      linewidth = line_width,
      linetype = line_type,
      alpha = fill_alpha,
      ...
    )
  } else {
    p <- p + geom_density(
      aes(
        x = {{x}},
        color = {{color}},
        fill = {{color}}
      ),
      na.rm = TRUE,
      linewidth = line_width,
      linetype = line_type,
      alpha = fill_alpha,
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
