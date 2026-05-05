
#' @title Initialize a plan for train-test split
#'
#' @description
#' Internal helper used by \code{make_split()} to determine the initial training
#' window size for each time series.
#'
#' @param main_frame A \code{tibble} containing the time series data.
#' @param context A named \code{list} with the identifiers for
#'   \code{series_id}, \code{value_id}, and \code{index_id}.
#' @param type The type for the initial split. Possible values are
#'   \code{"first"}, \code{"last"}, and \code{"prob"}.
#' @param value Numeric value specifying the split.
#'
#' @return
#' A \code{tibble} with one row per time series and columns for the total number
#' of observations and the initial training window size.
#'
#' @noRd

initialize_split <- function(main_frame,
                             context,
                             type = c("first", "last", "prob"),
                             value = NULL) {

  series_id <- context[["series_id"]]

  data <- main_frame |>
    select(all_of(series_id)) |>
    group_by(!!sym(series_id)) |>
    summarise(n_total = n(), .groups = "drop")

  # Case 1: Use the first value observations for training
  if (type == "first") {
    data <- data |>
      mutate(n_init = value)
  }

  # Case 2: Use the last value observations for testing
  if (type == "last") {
    data <- data |>
      mutate(n_init = .data$n_total - value - 1)
  }

  # Case 3: Use value pct. of observations for training
  if (type == "prob") {
    data <- data |>
      mutate(n_init = floor(value * .data$n_total))
  }

  return(data)
}



#' @title Create indices for train and test splits
#'
#' @description
#' Create train and test indices for time series cross-validation.
#'
#' @details
#' \code{split_index()} creates integer index vectors for rolling-origin
#' resampling. The function can create either fixed-window or expanding-window
#' splits:
#' \itemize{
#'   \item \code{mode = "slide"} creates a fixed training window that moves
#'   forward over time.
#'   \item \code{mode = "stretch"} creates an expanding training window that
#'   always starts at the first observation.
#' }
#'
#' The first training window contains \code{n_init} observations. Each test
#' window contains \code{n_ahead} observations. The argument \code{n_skip}
#' controls how many observations are skipped between consecutive split origins.
#' For example, with \code{n_ahead = 18} and \code{n_skip = 17}, consecutive
#' test windows are non-overlapping.
#'
#' If \code{n_lag > 0}, the test indices include lagged observations before the
#' forecast horizon. This is useful when lagged predictors are needed for
#' constructing features during testing.
#'
#' If \code{exceed = TRUE}, additional out-of-sample test indices are allowed to
#' exceed the original sample size.
#'
#' @param n_total Integer. The total number of observations in the time series.
#' @param n_init Integer. The number of observations in the initial training
#'   window.
#' @param n_ahead Integer. The forecast horizon, i.e. the number of observations
#'   in each test window.
#' @param n_skip Integer. The number of observations to skip between split
#'   origins. The default is \code{0}.
#' @param n_lag Integer. The number of lagged observations to include before the
#'   test window. The default is \code{0}.
#' @param mode Character value. Either \code{"slide"} for a fixed-window
#'   approach or \code{"stretch"} for an expanding-window approach.
#' @param exceed Logical value. If \code{TRUE}, test indices may exceed the
#'   original sample size.
#'
#' @return
#' A \code{list} with two elements:
#' \itemize{
#'   \item \code{train}: a list of integer vectors with training indices.
#'   \item \code{test}: a list of integer vectors with test indices.
#' }
#'
#' @family time series cross-validation
#' @export
#'
#' @examples
#' # Fixed-window splits
#' fixed_index <- split_index(
#'   n_total = 180,
#'   n_init = 120,
#'   n_ahead = 18,
#'   n_skip = 17,
#'   n_lag = 0,
#'   mode = "slide",
#'   exceed = FALSE
#' )
#'
#' fixed_index
#'
#' # Expanding-window splits
#' expanding_index <- split_index(
#'   n_total = 180,
#'   n_init = 120,
#'   n_ahead = 18,
#'   n_skip = 17,
#'   n_lag = 0,
#'   mode = "stretch",
#'   exceed = FALSE
#' )
#'
#' expanding_index

split_index <- function(n_total,
                        n_init,
                        n_ahead,
                        n_skip = 0,
                        n_lag = 0,
                        mode = "slide",
                        exceed = FALSE) {

  if (exceed) {
    n_total <- n_total + n_ahead
  }

  if (n_total < n_init + n_ahead) {
    stop(
      "There should be at least ",
      n_init + n_ahead,
      " observations in `data`",
      call. = FALSE
    )
  }

  if (!is.numeric(n_lag) | !(n_lag %% 1 == 0)) {
    stop("`n_lag` must be a whole number.", call. = FALSE)
  }

  if (n_lag > n_init) {
    stop(
      "`n_lag` must be less than or equal to the number of training observations.",
      call. = FALSE
    )
  }

  stops <- seq(n_init, (n_total - n_ahead), by = n_skip + 1)

  starts <- if (mode == "slide") {
    stops - n_init + 1
  } else {
    rep(1, length(stops))
  }

  # Prepare index vectors for training and testing as list
  train <- map2(.x = starts, .y = stops, ~seq(.x, .y))
  test <- map2(.x = stops + 1 - n_lag, .y = stops + n_ahead, ~seq(.x, .y))

  index <- list(
    train = train,
    test = test
  )

  return(index)
}



#' @title Expand the split frame
#'
#' @description
#' Internal helper used by \code{make_split()} to expand nested train and test
#' index lists into one row per time series and split.
#'
#' @param split_frame A \code{tibble} containing nested train and test indices.
#' @param context A named \code{list} with the identifiers for
#'   \code{series_id}, \code{value_id}, and \code{index_id}.
#'
#' @return
#' A \code{tibble} containing one row per time series and split.
#'
#' @noRd

expand_split <- function(split_frame,
                         context) {

  series_id <- context[["series_id"]]

  split_frame <- split_frame |>
    select(
      all_of(c(series_id, "n_splits", "train", "test"))
    ) |>
    group_by(!!sym(series_id)) |>
    mutate(split = list(seq_len(.data$n_splits))) |>
    ungroup() |>
    unnest(cols = all_of(c("split", "train", "test"))) |>
    select(
      all_of(c(series_id, "split", "train", "test"))
    )

  return(split_frame)
}



#' @title Create train-test splits for time series cross-validation
#'
#' @description
#' Create a split frame with train and test indices for one or more time series.
#'
#' @details
#' \code{make_split()} creates rolling-origin train-test splits for time series
#' cross-validation. The output is used by functions such as
#' \code{slice_train()} and \code{slice_test()} to extract the corresponding
#' training and testing samples from \code{main_frame}.
#'
#' The function supports two training-window modes:
#' \itemize{
#'   \item \code{mode = "slide"} creates a fixed-window approach. The training
#'   window has constant length and moves forward over time.
#'   \item \code{mode = "stretch"} creates an expanding-window approach. The
#'   training window starts at the first observation and grows over time.
#' }
#'
#' The initial training window is controlled by \code{type} and \code{value}:
#' \itemize{
#'   \item \code{type = "first"} uses the first \code{value} observations as the
#'   initial training window.
#'   \item \code{type = "last"} keeps the last \code{value} observations for
#'   testing and derives the initial training window from the remaining sample.
#'   \item \code{type = "prob"} uses \code{floor(value * n_total)} observations
#'   as the initial training window.
#' }
#'
#' The argument \code{n_skip} controls how far the rolling origin moves between
#' consecutive splits. For non-overlapping test windows, use
#' \code{n_skip = n_ahead - 1}.
#'
#' @param main_frame A \code{tibble} containing the time series data.
#' @param context A named \code{list} with the identifiers for
#'   \code{series_id}, \code{value_id}, and \code{index_id}.
#' @param type Character value. The type of initial split. Possible values are
#'   \code{"first"}, \code{"last"}, and \code{"prob"}.
#' @param value Numeric value specifying the initial split.
#' @param n_ahead Integer. The forecast horizon, i.e. the number of observations
#'   in each test window.
#' @param n_skip Integer. The number of observations to skip between split
#'   origins. The default is \code{0}.
#' @param n_lag Integer. The number of lagged observations to include before the
#'   test window. This is useful if lagged predictors are required when
#'   constructing test features. The default is \code{0}.
#' @param mode Character value. Either \code{"slide"} for a fixed-window
#'   approach or \code{"stretch"} for an expanding-window approach.
#' @param exceed Logical value. If \code{TRUE}, out-of-sample splits exceeding
#'   the original sample size are created.
#'
#' @return
#' A \code{tibble} containing the split plan. The output has one row per time
#' series and split, with list-columns \code{train} and \code{test} containing
#' integer row positions.
#'
#' @family time series cross-validation
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
#' main_frame <- M4_monthly_data |>
#'   filter(series == "M23100")
#'
#' # Fixed-window split plan
#' fixed_split <- make_split(
#'   main_frame = main_frame,
#'   context = context,
#'   type = "first",
#'   value = 120,
#'   n_ahead = 18,
#'   n_skip = 17,
#'   n_lag = 0,
#'   mode = "slide",
#'   exceed = FALSE
#' )
#'
#' fixed_split
#'
#' # Expanding-window split plan
#' expanding_split <- make_split(
#'   main_frame = main_frame,
#'   context = context,
#'   type = "first",
#'   value = 120,
#'   n_ahead = 18,
#'   n_skip = 17,
#'   n_lag = 0,
#'   mode = "stretch",
#'   exceed = FALSE
#' )
#'
#' expanding_split

make_split <- function(main_frame,
                       context,
                       type,
                       value,
                       n_ahead,
                       n_skip = 0,
                       n_lag = 0,
                       mode = "slide",
                       exceed = TRUE) {

  # Create initial split
  split_frame <- initialize_split(
    main_frame = main_frame,
    context = context,
    type = type,
    value = value
  )

  # Create indices for train and test data and add as nested list
  split_frame <- map_dfr(
    .x = seq_len(nrow(split_frame)),
    .f = ~{
      # Create indices for training and testing
      index <- split_index(
        n_total = split_frame$n_total[.x],
        n_init = split_frame$n_init[.x],
        n_ahead = n_ahead,
        n_skip = n_skip,
        n_lag = n_lag,
        mode = mode,
        exceed = exceed
      )

      # Add indices to split_frame
      split_frame |>
        slice(.x) |>
        mutate(n_ahead = n_ahead) |>
        mutate(n_splits = length(index$train)) |>
        mutate(train = list(index$train)) |>
        mutate(test = list(index$test))
    }
  )

  # Expand split_frame and return output
  split_frame <- expand_split(
    split_frame = split_frame,
    context = context
  )

  return(split_frame)
}
