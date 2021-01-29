
#' @title Split the data into training and testing
#'
#' @description This function splits the data into training and testing samples
#'   (i.e. partitioning into time slices) for time series cross-validation. The
#'   user can choose between \code{stretch} and \code{slide}. The first is an
#'   expanding window approach, while the latter is a fixed window approach.
#'   The user can define the window sizes for training and testing via
#'   \code{n_init} and \code{n_ahead}, as well as the step size for increments
#'   via \code{n_step}.
#'
#' @param data A valid \code{tsibble} in long format provided by the function \code{check_data()}.
#' @param n_init Integer value. The number of periods for the initial training window (must be positive).
#' @param n_ahead Integer value. The forecast horizon (n-steps-ahead, must be positive).
#' @param n_skip Integer value. The number of periods to skip between windows (must be zero or positive integer).
#' @param n_lag Integer value. A value to include a lag between the training and testing set. This is useful if lagged predictors will be used during training and testing.
#' @param mode Character value. Define the setup of the training window for time series cross validation. \code{stretch} is equivalent to an expanding window approach and \code{slide} is a fixed window approach.
#'
#' @return data A \code{tsibble} in the same format as the input data, but with the following additional columns:
#'    \itemize{
#'       \item{\code{sample}: Character value. Indicating whether the partition is training or testing.}
#'       \item{\code{split}: Integer value. The number of the time slice (training and testing).}
#'       \item{\code{id}: Integer value. The row number of the corresponding observations.}
#'       \item{\code{horizon}: Integer value. The forecast horizon (i.e. the size of the testing window. NAs for training.}
#'       }
#' @export

split_data <- function(data,
                       n_init,
                       n_ahead,
                       n_skip = 0,
                       n_lag = 0,
                       mode = "slide") {

  dttm <- index(data)
  variable <- key(data)

  data <- data %>%
    append_row(n = n_ahead) %>%
    as_tibble()

  time <- data %>%
    select(!!dttm) %>%
    distinct() %>%
    pull(!!dttm)

  n_total <- length(time)

  if (n_total < n_init + n_ahead)
    stop("There should be at least ",
         n_init + n_ahead,
         " observations in `data`",
         call. = FALSE)

  if (!is.numeric(n_lag) | !(n_lag%%1==0)) {
    stop("`n_lag` must be a whole number.", call. = FALSE)
  }

  if (n_lag > n_init) {
    stop("`n_lag` must be less than or equal to the number of training observations.", call. = FALSE)
  }

  stops <- seq(n_init, (n_total - n_ahead), by = n_skip + 1)
  starts <- if (mode == "slide") {
    stops - n_init + 1
  } else {
    starts <- rep(1, length(stops))
  }

  # Prepare index vectors for training and testing as list
  train_index <- map2(.x = starts, .y = stops, ~seq(.x, .y))
  test_index <- map2(.x = stops + 1 - n_lag, .y = stops + n_ahead, ~seq(.x, .y))

  # Slice train and test data, grouped by key variables
  train <- map(.x = 1:length(train_index), ~ {
    data %>%
      group_by(!!!variable) %>%
      slice(train_index[[.x]]) %>%
      mutate(split = .x) %>%
      mutate(id = train_index[[.x]]) %>%
      ungroup()
  })

  test <- map(.x = 1:length(test_index), ~ {
    data %>%
      group_by(!!!variable) %>%
      slice(test_index[[.x]]) %>%
      mutate(split = .x) %>%
      mutate(id = test_index[[.x]]) %>%
      ungroup()
  })

  # Flatten lists by row-wise binding
  train <- bind_rows(train)
  test <- bind_rows(test)

  # Finalize data (add columns for sample and horizon and convert to tsibble)
  train <- train %>%
    mutate(sample = "train") %>%
    mutate(horizon = NA_integer_) %>%
    as_tsibble(
      index = !!dttm,
      key = c(!!!variable, split))

  test <- test %>%
    mutate(sample = "test") %>%
    group_by(!!!variable, split) %>%
    mutate(horizon = row_number()) %>%
    ungroup() %>%
    as_tsibble(
      index = !!dttm,
      key = c(!!!variable, split))

  index <- tibble(
    train_index = train_index,
    test_index = test_index)

  list(
    train = train,
    test = test,
    index = index
  )
}
