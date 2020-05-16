
#' @title Split the data into training and testing.
#'
#' @description This function splits the data into training and testing (i.e. partitioning into time slices) for
#'    time series cross-validation. The user can choose between \code{stretch} and \code{slide}. The first is an
#'    expanding window approach, while the latter is a fixed window approach. The user can define the window sizes
#'    for training and testing via \code{n_init} and \code{n_ahead}, as well as the step size for increments via
#'    \code{n_step}.
#'
#' @param data A valid tsibble, either in long format provided by the function \code{clean_data(...)}.
#' @param mode Character value. Define the setup of the training window for time series cross validation. \code{stretch} is equivalent to an expanding window approach and \code{slide} is a fixed window approach.
#' @param n_init Integer value. The number of periods for the initial training window (must be positive).
#' @param n_step Integer value. The number of periods to skip between windows (must be positive).
#' @param n_ahead Integer value. The forecast horizon (n-steps-ahead, must be positive).
#'
#' @return data A tsibble with the same format like the input data, but with additional columns:
#'    \itemize{
#'       \item{\code{sample}: Character value. Indicating whether the partition is training or testing.}
#'       \item{\code{slice}: Integer value. The number of the time slice (training and testing).}
#'       \item{\code{horizon}: Integer value. The forecast horizon (i.e. the size of the testing window. NA for training.}
#'       \item{\code{type}: Character value. Indicating actual values, forecasts etc.}
#'       \item{\code{model}: Character value. The forecasting model.}
#'       }
#' @export

split_data <- function(data,
                       mode = "stretch",
                       n_init,
                       n_step,
                       n_ahead) {

  date_time <- index_var(data)
  variable <- key_vars(data)
  value <- measured_vars(data)

  # End date of the sample
  end_train <- data %>%
    pull(!!sym(date_time)) %>%
    unique() %>%
    last()

  # Extend data for out-of-sample forecasts
  # (measurement variables are set to NA)
  data <- data %>%
    update_tsibble(
      index = !!sym(date_time),
      key = c(!!!syms(variable))) %>%
    append_row(n = n_ahead) %>%
    mutate(type = "actual") %>%
    mutate(model = NA_character_)

  # Start date of first forecast (test slice one)
  start_test <- data %>%
    filter(row_number() == (n_init + 1)) %>%
    pull(!!sym(date_time))

  # Prepare training data .....................................................

  # Remove (enhanced) NAs
  data_train <- data %>%
    filter_index(. ~ end_train)

  # Roll up tsibble by tscv mode
  if (mode == "stretch") {
    data_train <- data_train %>%
      stretch_tsibble(
        .init = n_init,
        .step = n_step,
        .id = "slice")
  }

  if (mode == "slide") {
    data_train <- data_train %>%
      slide_tsibble(
        .size = n_init,
        .step = n_step,
        .id = "slice")
  }

  # Add columns sample and horizon
  data_train <- data_train %>%
    mutate(sample = "train") %>%
    mutate(horizon = NA_integer_)

  # Prepare test data .........................................................

  # Roll up tsibble
  data_test <- data %>%
    filter_index(start_test ~ .) %>%
    slide_tsibble(
      .size = n_ahead,
      .step = n_step,
      .id = "slice") %>%
    mutate(sample = "test") %>%
    group_by(slice, !!!syms(variable)) %>%
    mutate(horizon = row_number()) %>%
    ungroup()

  # Concatenate train and test data row-wise
  data <- rbind(data_train, data_test)

  # Adjust key variables
  data <- data %>%
    select(
      !!sym(date_time),
      !!!syms(variable),
      sample,
      slice,
      horizon,
      type,
      model,
      !!sym(value))

  return(data)
}
