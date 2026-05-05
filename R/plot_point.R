
#' @title Plot data as a scatterplot
#'
#' @description
#' Create a scatterplot for two variables.
#'
#' @details
#' \code{plot_point()} is a convenience wrapper around
#' \code{ggplot2::geom_point()}. It is useful for plotting relationships between
#' two variables, for example observed values over time, forecast errors by
#' horizon, or one numeric diagnostic against another.
#'
#' The arguments \code{x}, \code{y}, \code{facet_var}, and \code{color} are
#' passed as unquoted column names.
#'
#' If \code{color} is supplied, point colors are mapped to that variable and
#' \code{point_color} is ignored. If \code{color} is not supplied, all points are
#' drawn using \code{point_color}.
#'
#' Additional theme settings can be supplied through \code{theme_config}. This
#' should be a named list of arguments passed to \code{ggplot2::theme()}.
#'
#' @param data A \code{data.frame}, \code{tibble}, or \code{tsibble} in long
#'   format.
#' @param x Unquoted column in \code{data} used on the x-axis.
#' @param y Unquoted column in \code{data} containing numeric values shown on
#'   the y-axis.
#' @param facet_var Optional unquoted column in \code{data} used for faceting.
#' @param facet_scale Character value defining facet axis scaling. Common values
#'   are \code{"free"}, \code{"fixed"}, \code{"free_x"}, and \code{"free_y"}.
#' @param facet_nrow Optional integer. Number of rows in the facet layout.
#' @param facet_ncol Optional integer. Number of columns in the facet layout.
#' @param color Optional unquoted column in \code{data} used to map point color.
#' @param title Character value. Plot title.
#' @param subtitle Character value. Plot subtitle.
#' @param xlab Character value. Label for the x-axis.
#' @param ylab Character value. Label for the y-axis.
#' @param caption Character value. Plot caption.
#' @param point_size Numeric value defining the point size.
#' @param point_type Numeric or character value defining the point shape.
#' @param point_color Character value defining the point color. Ignored when
#'   \code{color} is supplied.
#' @param point_alpha Numeric value between \code{0} and \code{1} defining point
#'   transparency.
#' @param theme_set A complete \code{ggplot2} theme.
#' @param theme_config A named \code{list} with additional arguments passed to
#'   \code{ggplot2::theme()}.
#' @param ... Currently not used.
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
#'   filter(series == "M23100")
#'
#' plot_point(
#'   data = data,
#'   x = index,
#'   y = value,
#'   title = "M4 Monthly Time Series",
#'   subtitle = "Series M23100",
#'   xlab = "Time",
#'   ylab = "Value"
#' )
#'
#' acf_data <- estimate_acf(
#'   .data = M4_monthly_data |>
#'     filter(series %in% c("M23100", "M14395")),
#'   context = list(
#'     series_id = "series",
#'     value_id = "value",
#'     index_id = "index"
#'   ),
#'   lag_max = 12
#' )
#'
#' plot_point(
#'   data = acf_data,
#'   x = lag,
#'   y = value,
#'   color = series,
#'   title = "Autocorrelation by Series",
#'   subtitle = "Sample autocorrelation up to lag 12",
#'   xlab = "Lag",
#'   ylab = "ACF"
#' )

plot_point <- function(data,
                       x,
                       y,
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
                       point_size = 1.5,
                       point_type = 16,
                       point_color = "grey35",
                       point_alpha = 1,
                       theme_set = theme_tscv(),
                       theme_config = list(),
                       ...) {

  # Create initial ggplot object
  p <- ggplot(data = data)

  # Create points
  if (quo_is_null(enquo(color))) {
    p <- p + geom_point(
      aes(
        x = {{x}},
        y = {{y}}
      ),
      color = point_color,
      size = point_size,
      shape = point_type,
      alpha = point_alpha
    )
  } else {
    p <- p + geom_point(
      aes(
        x = {{x}},
        y = {{y}},
        color = {{color}}
      ),
      size = point_size,
      shape = point_type,
      alpha = point_alpha
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
