
#' @title Custom ggplot2 theme for tscv
#'
#' @description
#' Create the default \code{ggplot2} theme used by the \code{tscv} package.
#'
#' @details
#' \code{theme_tscv()} returns a complete \code{ggplot2} theme with a clean
#' layout, subtle grid lines, bottom legend placement, and formatting for plot
#' titles, subtitles, captions, facets, and axes.
#'
#' The theme is used as the default theme in the plotting helpers of the
#' \code{tscv} package, such as \code{plot_line()}, \code{plot_point()},
#' \code{plot_bar()}, \code{plot_histogram()}, \code{plot_density()}, and
#' \code{plot_qq()}.
#'
#' Since the returned object is a regular \code{ggplot2} theme, it can also be
#' added directly to any \code{ggplot} object with \code{+ theme_tscv()}.
#'
#' @param base_size Numeric value. Base font size.
#' @param base_family Character value. Base font family.
#' @param base_line_size Numeric value. Base line width for line elements.
#' @param base_rect_size Numeric value. Base line width for rectangle elements.
#'
#' @return
#' A complete \code{ggplot2} theme.
#'
#' @family data visualization
#' @export
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#'
#' data <- M4_monthly_data |>
#'   filter(series == "M23100") |>
#'   mutate(index = as.Date(index))
#'
#' # Plot with the default tscv theme
#' plot_line(
#'   data = data,
#'   x = index,
#'   y = value,
#'   title = "M4 Monthly Time Series",
#'   subtitle = "Series M23100",
#'   xlab = "Time",
#'   ylab = "Value",
#'   theme_set = theme_tscv()
#' )
#'
#' # The same plot with the default ggplot2 grey theme
#' plot_line(
#'   data = data,
#'   x = index,
#'   y = value,
#'   title = "M4 Monthly Time Series",
#'   subtitle = "Series M23100",
#'   xlab = "Time",
#'   ylab = "Value",
#'   theme_set = theme_grey()
#' )
#'
#' # theme_tscv() can also be added to regular ggplot objects
#' ggplot(data, aes(x = index, y = value)) +
#'   geom_line(color = "grey35") +
#'   labs(
#'     title = "M4 Monthly Time Series",
#'     subtitle = "Series M23100",
#'     x = "Time",
#'     y = "Value"
#'   ) +
#'   theme_tscv()

theme_tscv <- function(base_size = 11,
                       base_family = "",
                       base_line_size = base_size / 22,
                       base_rect_size = base_size / 22) {

  # The half-line (base-fontsize / 2) sets up the basic vertical
  # rhythm of the theme. Most margins will be set to this value.
  # However, when we work with relative sizes, we may want to multiply
  # `half_line` with the appropriate relative size. This applies in
  # particular for axis tick sizes. And also, for axis ticks and
  # axis titles, `half_size` is too large a distance, and we use `half_size / 2`
  # instead.
  half_line <- base_size / 2

  # Throughout the theme, we use three font sizes, `base_size` (`rel(1)`)
  # for normal, `rel(0.8)` for small, and `rel(1.2)` for large.

  ggplot2::theme(
    # Elements in this first block are not used directly, but are inherited by others.
    line = ggplot2::element_line(
      colour = "black",
      linewidth = base_line_size,
      linetype = 1,
      lineend = "butt"
    ),
    rect = ggplot2::element_rect(
      fill = "white",
      colour = "black",
      linewidth = base_rect_size,
      linetype = 1
    ),
    text = ggplot2::element_text(
      family = base_family,
      face = "plain",
      colour = "black",
      size = base_size,
      lineheight = 0.9,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      margin = ggplot2::margin(),
      debug = FALSE
    ),

    axis.line = ggplot2::element_blank(),
    axis.line.x = NULL,
    axis.line.y = NULL,
    axis.text = ggplot2::element_text(size = ggplot2::rel(0.8), colour = "grey30"),
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 0.8 * half_line / 2), vjust = 1),
    axis.text.x.top = ggplot2::element_text(margin = ggplot2::margin(b = 0.8 * half_line / 2), vjust = 0),
    axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 0.8 * half_line / 2), hjust = 1),
    axis.text.y.right = ggplot2::element_text(margin = ggplot2::margin(l = 0.8 * half_line / 2), hjust = 0),
    axis.ticks = ggplot2::element_blank(),
    axis.ticks.length = ggplot2::unit(half_line / 2, "pt"),
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = half_line / 2), vjust = 1),
    axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(b = half_line / 2), vjust = 0),
    axis.title.y = ggplot2::element_text(angle = 90, margin = ggplot2::margin(r = half_line / 2), vjust = 1),
    axis.title.y.right = ggplot2::element_text(angle = -90, margin = ggplot2::margin(l = half_line / 2), vjust = 0),

    legend.background = ggplot2::element_blank(),
    legend.spacing = ggplot2::unit(2 * half_line, "pt"),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.margin = ggplot2::margin(half_line, half_line, half_line, half_line),
    legend.key = ggplot2::element_blank(),
    legend.key.size = ggplot2::unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = ggplot2::element_text(size = ggplot2::rel(0.8)),
    legend.text.align = NULL,
    legend.title = ggplot2::element_blank(),
    legend.title.align = NULL,
    legend.position = "bottom",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,
    legend.box.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
    legend.box.background = ggplot2::element_blank(),
    legend.box.spacing = ggplot2::unit(2 * half_line, "pt"),

    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(color = "grey92", linewidth = 0.25),
    panel.grid.minor = ggplot2::element_line(color = "grey97", linewidth = 0.1),
    panel.spacing = ggplot2::unit(half_line, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.ontop = FALSE,

    strip.background = ggplot2::element_rect(colour = "grey97", fill = "grey97"),
    strip.text = ggplot2::element_text(
      colour = "grey10",
      size = ggplot2::rel(0.8),
      margin = ggplot2::margin(
        0.8 * half_line,
        0.8 * half_line,
        0.8 * half_line,
        0.8 * half_line
      )
    ),
    strip.text.x = NULL,
    strip.text.y = ggplot2::element_text(angle = -90),
    strip.placement = "inside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    strip.switch.pad.grid = ggplot2::unit(half_line / 2, "pt"),
    strip.switch.pad.wrap = ggplot2::unit(half_line / 2, "pt"),

    plot.background = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(
      size = ggplot2::rel(1.2),
      hjust = 0,
      vjust = 1,
      margin = ggplot2::margin(b = half_line),
      face = "bold"
    ),
    plot.subtitle = ggplot2::element_text(
      hjust = 0,
      vjust = 1,
      margin = ggplot2::margin(b = half_line),
      color = "grey40",
      face = "plain"
    ),
    plot.caption = ggplot2::element_text(
      size = ggplot2::rel(0.6),
      hjust = 1,
      vjust = 1,
      margin = ggplot2::margin(t = half_line),
      color = "gray40"
    ),
    plot.tag = ggplot2::element_text(size = ggplot2::rel(1.2), hjust = 0.5, vjust = 0.5),
    plot.tag.position = "topleft",
    plot.margin = ggplot2::margin(half_line, half_line, half_line, half_line),

    complete = TRUE
  )
}
