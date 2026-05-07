
# Definition of tscv colors
tscv_colors <- c(
  "steelblue"  = "#4682B4",
  "orange"     = "#E69F00",
  "green"      = "#009E73",
  "purple"     = "#7B3294",
  "red"        = "#D55E00",
  "skyblue"    = "#56B4E9",
  "yellow"     = "#F0E442",
  "dark grey"  = "#4D4D4D",
  "light grey" = "#D9D9D9"
)


#' @title Extract tscv colors
#'
#' @description
#' Extract named colors from the \code{tscv} color palette as hexadecimal color
#' codes.
#'
#' @details
#' \code{tscv_cols()} returns the hexadecimal color codes used by the
#' \code{tscv} package. If no color names are supplied, all available colors are
#' returned.
#'
#' Available colors are:
#' \code{"red"}, \code{"green"}, \code{"blue"}, \code{"orange"},
#' \code{"yellow"}, \code{"light grey"}, and \code{"dark grey"}.
#'
#' @param ... Character values giving the names of colors to extract.
#'
#' @return
#' A named character vector of hexadecimal color codes.
#'
#' @family data visualization
#' @export
#'
#' @examples
#' # Return all available tscv colors
#' tscv_cols()
#'
#' # Return selected colors
#' tscv_cols("steelblue", "orange", "green")
#'
#' # Use a tscv color in a plot
#' library(dplyr)
#'
#' data <- M4_monthly_data |>
#'   filter(series == "M23100")
#'
#' plot_line(
#'   data = data,
#'   x = index,
#'   y = value,
#'   title = "M4 Monthly Time Series",
#'   subtitle = "Series M23100",
#'   xlab = "Time",
#'   ylab = "Value",
#'   line_color = tscv_cols("steelblue")
#' )

tscv_cols <- function(...) {

  cols <- c(...)

  if (is.null(cols)) {
    return(tscv_colors)
  }

  if (!all(cols %in% names(tscv_colors))) {
    stop(
      "Unknown tscv color name. Available colors are: ",
      paste(names(tscv_colors), collapse = ", "),
      call. = FALSE
    )
  }

  tscv_colors[cols]
}



# Definition of tscv color palettes
tscv_palettes <- list(
  "main" = tscv_cols("steelblue", "orange", "green"),
  "cool" = tscv_cols("steelblue", "skyblue", "green"),
  "hot" = tscv_cols("orange", "red", "yellow"),
  "mixed" = tscv_cols("steelblue", "orange", "green", "purple", "skyblue", "red"),
  "grey" = tscv_cols("light grey", "dark grey")
)



#' @title Create a tscv color palette
#'
#' @description
#' Create a color interpolation function based on one of the predefined
#' \code{tscv} palettes.
#'
#' @details
#' \code{tscv_pal()} returns a palette function created with
#' \code{grDevices::colorRampPalette()}. The returned function can be used to
#' generate any number of colors from the selected palette.
#'
#' Available palettes are:
#' \itemize{
#'   \item \code{"main"}: blue, green, yellow.
#'   \item \code{"cool"}: blue, green.
#'   \item \code{"hot"}: yellow, orange, red.
#'   \item \code{"mixed"}: blue, green, yellow, orange, red.
#'   \item \code{"grey"}: light grey, dark grey.
#' }
#'
#' @param palette Character value. Name of the palette.
#' @param reverse Logical value. If \code{TRUE}, the palette is reversed.
#' @param ... Additional arguments passed to
#'   \code{grDevices::colorRampPalette()}.
#'
#' @return
#' A palette function that takes an integer and returns hexadecimal color codes.
#'
#' @family data visualization
#' @export
#'
#' @examples
#' # Create a palette function
#' pal <- tscv_pal("main")
#'
#' # Generate five colors
#' pal(5)
#'
#' # Reverse the palette
#' tscv_pal("hot", reverse = TRUE)(5)
#'
#' # Use generated colors in base R
#' barplot(
#'   height = c(3, 5, 4),
#'   col = tscv_pal("main")(3)
#' )

tscv_pal <- function(palette = "main",
                     reverse = FALSE,
                     ...) {

  if (!palette %in% names(tscv_palettes)) {
    stop(
      "Unknown tscv palette. Available palettes are: ",
      paste(names(tscv_palettes), collapse = ", "),
      call. = FALSE
    )
  }

  pal <- tscv_palettes[[palette]]

  if (reverse) {
    pal <- rev(pal)
  }

  colorRampPalette(pal, ...)
}



#' @title Create a tscv color scale
#'
#' @description
#' Create a \code{ggplot2} color scale based on a predefined \code{tscv} palette.
#'
#' @details
#' \code{scale_color_tscv()} creates either a discrete or continuous color scale
#' for the \code{color} aesthetic.
#'
#' For discrete variables, the function uses \code{ggplot2::discrete_scale()}.
#' For continuous variables, it uses \code{ggplot2::scale_color_gradientn()}.
#'
#' Available palettes are \code{"main"}, \code{"cool"}, \code{"hot"},
#' \code{"mixed"}, and \code{"grey"}.
#'
#' @param palette Character value. Name of the palette.
#' @param discrete Logical value. If \code{TRUE}, create a discrete color scale.
#'   If \code{FALSE}, create a continuous color scale.
#' @param reverse Logical value. If \code{TRUE}, the palette is reversed.
#' @param ... Additional arguments passed to \code{ggplot2::discrete_scale()} or
#'   \code{ggplot2::scale_color_gradientn()}.
#'
#' @return
#' A \code{ggplot2} scale object.
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
#' plot_line(
#'   data = data,
#'   x = index,
#'   y = value,
#'   color = series,
#'   title = "M4 Monthly Time Series",
#'   subtitle = "Selected monthly series",
#'   xlab = "Time",
#'   ylab = "Value"
#' ) +
#'   scale_color_tscv(palette = "main")

scale_color_tscv <- function(palette = "main",
                             discrete = TRUE,
                             reverse = FALSE,
                             ...) {

  pal <- tscv_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale(
      aesthetics = "colour",
      palette = pal,
      ...
    )
  } else {
    scale_color_gradientn(
      colours = pal(256),
      ...
    )
  }
}



#' @title Create a tscv fill scale
#'
#' @description
#' Create a \code{ggplot2} fill scale based on a predefined \code{tscv} palette.
#'
#' @details
#' \code{scale_fill_tscv()} creates either a discrete or continuous fill scale
#' for the \code{fill} aesthetic.
#'
#' For discrete variables, the function uses \code{ggplot2::discrete_scale()}.
#' For continuous variables, it uses \code{ggplot2::scale_fill_gradientn()}.
#'
#' Available palettes are \code{"main"}, \code{"cool"}, \code{"hot"},
#' \code{"mixed"}, and \code{"grey"}.
#'
#' @param palette Character value. Name of the palette.
#' @param discrete Logical value. If \code{TRUE}, create a discrete fill scale.
#'   If \code{FALSE}, create a continuous fill scale.
#' @param reverse Logical value. If \code{TRUE}, the palette is reversed.
#' @param ... Additional arguments passed to \code{ggplot2::discrete_scale()} or
#'   \code{ggplot2::scale_fill_gradientn()}.
#'
#' @return
#' A \code{ggplot2} scale object.
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
#'   color = series,
#'   title = "Average Value by Series",
#'   xlab = "Series",
#'   ylab = "Mean"
#' ) +
#'   scale_fill_tscv(palette = "main")

scale_fill_tscv <- function(palette = "main",
                            discrete = TRUE,
                            reverse = FALSE,
                            ...) {

  pal <- tscv_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale(
      aesthetics = "fill",
      palette = pal,
      ...
    )
  } else {
    scale_fill_gradientn(
      colours = pal(256),
      ...
    )
  }
}
