
#' @title Convert data to a tsibble
#'
#' @description
#' Convert a \code{tibble} containing time series data to a \code{tsibble}.
#'
#' @details
#' \code{make_tsibble()} is a small helper for time series cross-validation
#' workflows. It uses the time index and series identifier supplied in
#' \code{context} to create a regular \code{tsibble}.
#'
#' The input data must contain the columns specified by \code{context$index_id}
#' and \code{context$series_id}. The column specified by \code{context$index_id}
#' is used as the time index, and the column specified by
#' \code{context$series_id} is used as the key.
#'
#' @param main_frame A \code{tibble} containing the time series data.
#' @param context A named \code{list} with the identifiers for
#'   \code{series_id}, \code{value_id}, and \code{index_id}.
#'
#' @return
#' A \code{tsibble} with the same columns as \code{main_frame}, using the index
#' and key defined in \code{context}.
#'
#' @family time series cross-validation
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tsibble)
#'
#' context <- list(
#'   series_id = "series",
#'   value_id = "value",
#'   index_id = "index"
#' )
#'
#' main_frame <- M4_monthly_data |>
#'   filter(series %in% c("M23100", "M14395"))
#'
#' tsibble_frame <- make_tsibble(
#'   main_frame = main_frame,
#'   context = context
#' )
#'
#' tsibble_frame

make_tsibble <- function(main_frame, context) {

  index_id <- context[["index_id"]]
  series_id <- context[["series_id"]]

  main_frame <- main_frame |>
    as_tsibble(
      index = !!sym(index_id),
      key = !!sym(series_id)
    )

  return(main_frame)
}
