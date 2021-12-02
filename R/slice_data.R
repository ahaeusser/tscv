
#' @title Slice the train data from the complete data
#'
#' @description \code{slice_train} creates the train data from the
#'   complete data set \code{main_frame} according to \code{split_frame}. Same
#'   columns as \code{main_frame}, but the number of the split is added as
#'   integer.
#'
#' @param main_frame A \code{tibble} containing the time series data.
#' @param split_frame A \code{tibble} containing the splits into train and test.
#'   The result of a call to \code{make_split()}.
#' @param context A named \code{list} with the identifiers for \code{seried_id},
#'   \code{value_id} and \code{index_id}.
#'
#' @return train_frame is a \code{tibble} containing the train data.
#' @export

slice_train <- function(main_frame,
                        split_frame,
                        context) {

  series_id <- context[["series_id"]]

  train_frame <- map_dfr(
    .x = 1:nrow(split_frame),
    .f = ~{
      main_frame %>%
        filter(!!sym(series_id) == split_frame[[series_id]][.x]) %>%
        slice(split_frame[["train"]][[.x]]) %>%
        mutate(split = split_frame[["split"]][[.x]])
    }
  )
  return(train_frame)
}


#' @title Slice the test data from the complete data
#'
#' @description \code{slice_test} creates the test data from the
#'   complete data set \code{main_frame} according to \code{split_frame}. Same
#'   columns as \code{main_frame}, but the number of the split is added as
#'   integer.
#'
#' @param main_frame A \code{tibble} containing the time series data.
#' @param split_frame A \code{tibble} containing the splits into train and test.
#'   The result of a call to \code{make_split()}.
#' @param context A named \code{list} with the identifiers for \code{seried_id},
#'   \code{value_id} and \code{index_id}.
#'
#' @return test_frame is a \code{tibble} containing the test data.
#' @export

slice_test <- function(main_frame,
                       split_frame,
                       context) {

  series_id <- context[["series_id"]]

  test_frame <- map_dfr(
    .x = 1:nrow(split_frame),
    .f = ~{
      main_frame %>%
        filter(!!sym(series_id) == split_frame[[series_id]][.x]) %>%
        slice(split_frame[["test"]][[.x]]) %>%
        mutate(split = split_frame[["split"]][[.x]])
    }
  )
  return(test_frame)
}
