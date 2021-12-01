
#' @title Calculate forecast errors
#'
#' @description \code{errors} calculates the forecast errors (error) and
#'   percentage forecast errors (pct_error).
#'
#' @param future_frame A \code{tibble} containing the forecasts for the models, splits, etc.
#' @param main_frame A \code{tibble} containing the actual values.
#' @param series_id Character value specifying the series identifier.
#' @param value_id Character value specifying the value identifier.
#' @param index_id Character value specifying the index identifier.
#'
#' @return errors A \code{tibble} with forecast errors and
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
  errors <- left_join(
    x = future_frame,
    y = main_frame,
    by = c(series_id, index_id)) %>%
    select(c(!!sym(series_id), "model", "split", "horizon", "point", "actual")) %>%
    mutate(error = .data$actual - .data$point) %>%
    mutate(pct_error = ((.data$actual - .data$point) / .data$actual) * 100) %>%
    select(-c(.data$point, .data$actual))

  return(errors)
}
