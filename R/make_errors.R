
#' @title Calculate forecast errors and percentage errors
#'
#' @description \code{make_errors} calculates the forecast errors (error) and
#'   percentage forecast errors (pct_error).
#'
#'    \itemize{
#'       \item{\code{series_id}: Unique identifier for the time series as specified in \code{context}}.
#'       \item{\code{model}: Character value. The forecasting model.}
#'       \item{\code{split}: Integer value. The number of the train data split.}
#'       \item{\code{horizon}: The forecast horizon as integer.}
#'       \item{\code{error}: The forecast errors as numeric value.}
#'       \item{\code{pct_error}: The percentage forecast errors as numeric value.}
#'       }
#'
#' @param future_frame A \code{tibble} containing the forecasts for the models, splits, etc.
#' @param main_frame A \code{tibble} containing the actual values.
#' @param context A named \code{list} with the identifiers for \code{seried_id}, \code{value_id} and \code{index_id}.
#'
#' @return error_frame is a \code{tibble} with forecast errors and
#'   percentage forecast errors.
#' @export

make_errors <- function(future_frame,
                        main_frame,
                        context) {

  series_id <- context[["series_id"]]
  value_id <- context[["value_id"]]
  index_id <- context[["index_id"]]

  # Prepare test data
  main_frame <- rename(
    .data = main_frame,
    actual = !!sym(value_id)
  )

  # Join test data and forecasts and calculate forecast errors and
  # percentage errors
  error_frame <- left_join(
    x = future_frame,
    y = main_frame,
    by = c(series_id, index_id)) %>%
    select(c(!!sym(series_id), "model", "split", "horizon", "point", "actual")) %>%
    mutate(error = .data$actual - .data$point) %>%
    mutate(pct_error = ((.data$actual - .data$point) / .data$actual) * 100) %>%
    select(-c(.data$point, .data$actual))

  return(error_frame)
}
