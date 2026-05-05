
#' @title Plot data as a bar chart
#'
#' @description
#' Create a bar chart for grouped numeric values.
#'
#' @details
#' \code{plot_bar()} is a convenience wrapper around \code{ggplot2::geom_bar()}
#' with \code{stat = "identity"}. It is intended for data that already contains
#' summarized values, for example accuracy metrics, counts, or grouped summary
#' statistics.
#'
#' The arguments \code{x}, \code{y}, \code{facet_var}, and \code{color} are
#' passed as unquoted column names.
#'
#' If \code{color} is supplied, bar fill colors are mapped to that variable and
#' \code{bar_color} is ignored. If \code{color} is not supplied, all bars are
#' drawn using \code{bar_color}.
#'
#' The argument \code{position} controls how bars are displayed when
#' \code{color} is supplied. Common values are \code{"dodge"} and
#' \code{"stack"}.
#'
#' If \code{flip = TRUE}, the x-axis and y-axis are swapped using
#' \code{ggplot2::coord_flip()}.
#'
#' If \code{reorder = TRUE}, \code{tidytext::scale_x_reordered()} is added.
#' This is useful when the x-axis has been reordered within facets with
#' \code{tidytext::reorder_within()}.
#'
#' Additional theme settings can be supplied through \code{theme_config}. This
#' should be a named list of arguments passed to \code{ggplot2::theme()}.
#'
#' @param data A \code{data.frame}, \code{tibble}, or \code{tsibble} in long
#'   format.
#' @param x Unquoted column in \code{data} used on the x-axis.
#' @param y Unquoted column in \code{data} containing numeric values shown on
#'   the y-axis.
#' @param position Character value defining the bar position. Common values are
#'   \code{"dodge"} and \code{"stack"}.
#' @param facet_var Optional unquoted column in \code{data} used for faceting.
#' @param facet_scale Character value defining facet axis scaling. Common values
#'   are \code{"free"}, \code{"fixed"}, \code{"free_x"}, and \code{"free_y"}.
#' @param facet_nrow Optional integer. Number of rows in the facet layout.
#' @param facet_ncol Optional integer. Number of columns in the facet layout.
#' @param color Optional unquoted column in \code{data} used to map bar fill
#'   color.
#' @param flip Logical value. If \code{TRUE}, the plot is flipped using
#'   \code{ggplot2::coord_flip()}.
#' @param reorder Logical value. If \code{TRUE}, add
#'   \code{tidytext::scale_x_reordered()} for reordered facet labels.
#' @param title Character value. Plot title.
#' @param subtitle Character value. Plot subtitle.
#' @param xlab Character value. Label for the x-axis.
#' @param ylab Character value. Label for the y-axis.
#' @param caption Character value. Plot caption.
#' @param bar_size Numeric value defining the bar border line width.
#' @param bar_color Character value defining the bar fill color. Ignored when
#'   \code{color} is supplied.
#' @param bar_alpha Numeric value between \code{0} and \code{1} defining bar
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
#' context <- list(
#'   series_id = "series",
#'   value_id = "value",
#'   index_id = "index"
#' )
#'
#' data <- M4_monthly_data |>
#'   filter(series %in% c("M23100", "M14395"))
#'
#' stats <- summarise_stats(
#'   .data = data,
#'   context = context
#' )
#'
#' plot_bar(
#'   data = stats,
#'   x = series,
#'   y = mean,
#'   title = "Average Value by Series",
#'   xlab = "Series",
#'   ylab = "Mean"
#' )
#'
#' acf_data <- estimate_acf(
#'   .data = data,
#'   context = context,
#'   lag_max = 12
#' )
#'
#' plot_bar(
#'   data = acf_data,
#'   x = lag,
#'   y = value,
#'   facet_var = series,
#'   title = "Autocorrelation by Series",
#'   subtitle = "Sample autocorrelation up to lag 12",
#'   xlab = "Lag",
#'   ylab = "ACF"
#' )

plot_bar <- function(data,
                     x,
                     y,
                     position = "dodge",
                     facet_var = NULL,
                     facet_scale = "free",
                     facet_nrow = NULL,
                     facet_ncol = NULL,
                     color = NULL,
                     flip = FALSE,
                     reorder = FALSE,
                     title = NULL,
                     subtitle = NULL,
                     xlab = NULL,
                     ylab = NULL,
                     caption = NULL,
                     bar_size = 0.75,
                     bar_color = "grey35",
                     bar_alpha = 1,
                     theme_set = theme_tscv(),
                     theme_config = list(),
                     ...) {

  # Create initial ggplot object
  p <- ggplot(data = data)

  # Create bars
  if (quo_is_null(enquo(color))) {
    p <- p + geom_bar(
      stat = "identity",
      position = position,
      aes(
        x = {{x}},
        y = {{y}}),
      fill = bar_color,
      size = bar_size,
      alpha = bar_alpha
    )
  } else {
    p <- p + geom_bar(
      stat = "identity",
      position = position,
      aes(
        x = {{x}},
        y = {{y}},
        fill = {{color}}),
      size = bar_size,
      alpha = bar_alpha
    )
  }

  # Create facet
  if (!quo_is_null(enquo(facet_var))) {
    p <- p + facet_wrap(
      facets = vars({{facet_var}}),
      scales = facet_scale,
      nrow = facet_nrow,
      ncol = facet_ncol)

    if (reorder == TRUE) {
      p <- p + scale_x_reordered()
    }
  }

  # Flip plot
  if (flip == TRUE) {
    p <- p + coord_flip()
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
