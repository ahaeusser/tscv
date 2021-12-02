
#' @title Convert the forecasts from a \code{fable} to a \code{future_frame}
#'
#' @description \code{make_future} converts the forecasts from a \code{fable}
#'   to a \code{future_frame}. A \code{future_frame} is a tibble with a
#'   standardized format and contains the columns:
#'
#'    \itemize{
#'       \item{\code{index_id}: Date-time index as specified in \code{context}}.
#'       \item{\code{series_id}: Unique identifier for the time series as specified in \code{context}}.
#'       \item{\code{model}: Character value. The forecasting model.}
#'       \item{\code{split}: Integer value. The number of the train data split.}
#'       \item{\code{horizon}: The forecast horizon as integer.}
#'       \item{\code{point}: The point forecast as numeric value.}
#'       }
#'
#' @param fable A \code{fable} created via \code{fabletools::forecast()}.
#' @param context A named \code{list} with the identifiers for \code{seried_id}, \code{value_id} and \code{index_id}.
#'
#' @return future_frame is a \code{tibble} with the forecasts.
#' @export

make_future <- function(fable,
                        context) {

  series_id <- context[["series_id"]]
  value_id <- context[["value_id"]]
  index_id <- context[["index_id"]]

  future_frame <- fable%>%
    group_by_key() %>%
    mutate(horizon = 1:n()) %>%
    ungroup() %>%
    as_tibble() %>%
    select(-!!sym(value_id)) %>%
    rename(point = .mean) %>%
    rename(model = .model) %>%
    select(
      !!sym(index_id),
      !!sym(series_id),
      model,
      split,
      horizon,
      everything())

  return(future_frame)
}
