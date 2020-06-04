

# Definition of tscv colors
tscv_colors <- c(
  `red`        = "#d11141",
  `green`      = "#00b159",
  `blue`       = "#00aedb",
  `orange`     = "#f37735",
  `yellow`     = "#ffc425",
  `light grey` = "#cccccc",
  `dark grey`  = "#8c8c8c"
  )


#' @title Function to extract tscv colors as hex codes.
#'
#' @description Function to extract tscv colors as hex codes.
#'
#' @param ... Character names of tscv_colors.

tscv_cols <- function(...) {

  cols <- c(...)
  if (is.null(cols))
    return (tscv_colors)
  tscv_colors[cols]
}


# Definition of tscv color palettes
tscv_palettes <- list(
  `main`  = tscv_cols("blue", "green", "yellow"),
  `cool`  = tscv_cols("blue", "green"),
  `hot`   = tscv_cols("yellow", "orange", "red"),
  `mixed` = tscv_cols("blue", "green", "yellow", "orange", "red"),
  `grey`  = tscv_cols("light grey", "dark grey")
)


#' @title Return function to interpolate a tscv color palette.
#'
#' @description Return function to interpolate a tscv color palette.
#'
#' @param palette Character value. The name of the palette in tscv_palettes.
#' @param reverse Logical value indicating whether the palette should be reversed.
#' @param ... Additional arguments to pass to colorRampPalette()

tscv_pal <- function(palette = "main",
                     reverse = FALSE,
                     ...) {

  pal <- tscv_palettes[[palette]]

  if (reverse) pal <- rev(pal)
  colorRampPalette(pal, ...)
}



#' @title Color scale constructor for tscv colors.
#'
#' @description Color scale constructor for tscv colors.
#'
#' @param palette Character value. The name of the palette in tscv_palettes.
#' @param discrete Logical value indicating whether color aesthetic is discrete or not.
#' @param reverse Logical value indicating whether the palette should be reversed.
#' @param ... Additional arguments passed to \code{discrete_scale()} or
#'    \code{scale_color_gradientn()}.
#'
#' @export

scale_color_tscv <- function(palette = "main",
                             discrete = TRUE,
                             reverse = FALSE,
                             ...) {

  pal <- tscv_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("tscv_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}



#' @title Fill scale constructor for tscv colors.
#'
#' @description Fill scale constructor for tscv colors.
#'
#' @param palette Character value. The name of the palette in tscv_palettes.
#' @param discrete Logical value indicating whether color aesthetic is discrete or not.
#' @param reverse Logical value indicating whether the palette should be reversed.
#' @param ... Additional arguments passed to \code{discrete_scale()} or
#'    \code{scale_fill_gradientn()}.
#'
#' @export

scale_fill_tscv <- function(palette = "main",
                            discrete = TRUE,
                            reverse = FALSE,
                            ...) {

  pal <- tscv_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("tscv_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
