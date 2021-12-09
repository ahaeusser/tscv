
#' @title Convert tibble to tsibble
#'
#' @description \code{make_tsibble} is a helper function to convert a
#'   \code{tibble} to a \code{tsibble}.
#'
#' @param main_frame A \code{tibble} containing the time series data.
#' @param context A named \code{list} with the identifiers for \code{seried_id}, \code{value_id} and \code{index_id}.
#'
#' @return main_frame Same structure as before, just stored as \code{tsibble}.
#' @export

make_tsibble <- function(main_frame, context) {

  index_id <- context[["index_id"]]
  series_id <- context[["series_id"]]

  main_frame <- main_frame %>%
    as_tsibble(
      index = !!sym(index_id),
      key = !!sym(series_id)
    )

  return(main_frame)
}
