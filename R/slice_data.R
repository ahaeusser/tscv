
#' @title Slice training data from a split frame
#'
#' @description
#' Extract training observations from a complete time series data set according
#' to a split plan created by \code{make_split()}.
#'
#' @details
#' \code{slice_train()} uses the row positions stored in the \code{train}
#' list-column of \code{split_frame} to extract the corresponding observations
#' from \code{main_frame}. The function is designed for rolling-origin time
#' series cross-validation workflows.
#'
#' The returned data has the same columns as \code{main_frame}, plus a
#' \code{split} column identifying the train-test split. If \code{main_frame}
#' contains multiple time series, slicing is performed separately for each
#' series using the series identifier supplied in \code{context}.
#'
#' @param main_frame A \code{tibble} containing the complete time series data.
#' @param split_frame A \code{tibble} containing train and test indices, usually
#'   created by \code{make_split()}.
#' @param context A named \code{list} with the identifiers for
#'   \code{series_id}, \code{value_id}, and \code{index_id}.
#'
#' @return
#' A \code{tibble} containing the sliced training data. It contains the same
#' columns as \code{main_frame}, plus a \code{split} column.
#'
#' @family time series cross-validation
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' context <- list(
#'   series_id = "bidding_zone",
#'   value_id = "value",
#'   index_id = "time"
#' )
#'
#' main_frame <- elec_price |>
#'   filter(bidding_zone %in% c("DE", "FR"))
#'
#' split_frame <- make_split(
#'   main_frame = main_frame,
#'   context = context,
#'   type = "first",
#'   value = 2400,
#'   n_ahead = 24,
#'   n_skip = 23,
#'   n_lag = 0,
#'   mode = "stretch",
#'   exceed = FALSE
#' )
#'
#' train_frame <- slice_train(
#'   main_frame = main_frame,
#'   split_frame = split_frame,
#'   context = context
#' )
#'
#' train_frame

slice_train <- function(main_frame,
                        split_frame,
                        context) {

  series_id <- context[["series_id"]]

  train_frame <- map_dfr(
    .x = seq_len(nrow(split_frame)),
    .f = ~{
      series_value <- split_frame[[series_id]][.x]
      train_index <- split_frame[["train"]][[.x]]
      split_value <- split_frame[["split"]][[.x]]

      main_frame |>
        filter(!!sym(series_id) == series_value) |>
        slice(train_index) |>
        mutate(split = split_value)
    }
  )

  return(train_frame)
}



#' @title Slice test data from a split frame
#'
#' @description
#' Extract test observations from a complete time series data set according to a
#' split plan created by \code{make_split()}.
#'
#' @details
#' \code{slice_test()} uses the row positions stored in the \code{test}
#' list-column of \code{split_frame} to extract the corresponding observations
#' from \code{main_frame}. The function is designed for rolling-origin time
#' series cross-validation workflows.
#'
#' The returned data has the same columns as \code{main_frame}, plus a
#' \code{split} column identifying the train-test split. If \code{main_frame}
#' contains multiple time series, slicing is performed separately for each
#' series using the series identifier supplied in \code{context}.
#'
#' When \code{make_split()} was called with \code{n_lag > 0}, the test data may
#' include lagged observations before the forecast horizon.
#'
#' @param main_frame A \code{tibble} containing the complete time series data.
#' @param split_frame A \code{tibble} containing train and test indices, usually
#'   created by \code{make_split()}.
#' @param context A named \code{list} with the identifiers for
#'   \code{series_id}, \code{value_id}, and \code{index_id}.
#'
#' @return
#' A \code{tibble} containing the sliced test data. It contains the same columns
#' as \code{main_frame}, plus a \code{split} column.
#'
#' @family time series cross-validation
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' context <- list(
#'   series_id = "bidding_zone",
#'   value_id = "value",
#'   index_id = "time"
#' )
#'
#' main_frame <- elec_price |>
#'   filter(bidding_zone %in% c("DE", "FR"))
#'
#' split_frame <- make_split(
#'   main_frame = main_frame,
#'   context = context,
#'   type = "first",
#'   value = 2400,
#'   n_ahead = 24,
#'   n_skip = 23,
#'   n_lag = 0,
#'   mode = "stretch",
#'   exceed = FALSE
#' )
#'
#' test_frame <- slice_test(
#'   main_frame = main_frame,
#'   split_frame = split_frame,
#'   context = context
#' )
#'
#' test_frame

slice_test <- function(main_frame,
                       split_frame,
                       context) {

  series_id <- context[["series_id"]]

  test_frame <- map_dfr(
    .x = seq_len(nrow(split_frame)),
    .f = ~{
      series_value <- split_frame[[series_id]][.x]
      test_index <- split_frame[["test"]][[.x]]
      split_value <- split_frame[["split"]][[.x]]

      main_frame |>
        filter(!!sym(series_id) == series_value) |>
        slice(test_index) |>
        mutate(split = split_value)
    }
  )

  return(test_frame)
}
