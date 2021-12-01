
#' @title Initialize a plan for train-test split
#'
#' @description The function creates the initial split into training and
#'   testing data.
#'
#' @param data A \code{tibble} containing the actual values.
#' @param context A \code{list} with \code{series_id}, \code{value_id} and \code{index_id}.
#' @param type Character value. The type for the initial split.
#' @param value Numeric value.
#'
#' @return A \code{tibble}
#' @export

initialize_split <- function(main,
                             context,
                             type = c("first", "last", "prob"),
                             value = NULL) {

  series_id <- context[["series_id"]]

  data <- main %>%
    select(!!sym(series_id)) %>%
    group_by(!!sym(series_id)) %>%
    summarise(n_total = n()) %>%
    ungroup()

  # Case 1: Use the first value observations for training
  if (type == "first") {
    data <- data %>%
      mutate(n_init = value)
  }

  # Case 2: Use the last value observations for testing
  if (type == "last") {
    data <- data %>%
      mutate(n_init = n_total - value - 1)
  }

  # Case 3: Use value pct. of observations for training
  if (type == "prob") {
    data <- data %>%
      mutate(n_init = floor(value * n_total))
  }

  return(data)
}




#' @title Create indices for train and test splits.
#'
#' @description ...
#'
#' @param n_total Integer value. The total number of observations.
#' @param n_init Integer value. The number of periods for the initial training window (must be positive).
#' @param n_ahead Integer value. The forecast horizon (n-steps-ahead, must be positive).
#' @param n_skip Integer value. The number of periods to skip between windows (must be zero or positive integer).
#' @param n_lag Integer value. A value to include a lag between the training and testing set. This is useful if lagged predictors will be used during training and testing.
#' @param mode Character value. Define the setup of the training window for time series cross validation. \code{stretch} is equivalent to an expanding window approach and \code{slide} is a fixed window approach.
#' @param exceed Logical value.
#'
#' @return ...
#' @export

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
  train <- map2(.x = starts, .y = stops, ~seq(.x, .y))
  test <- map2(.x = stops + 1 - n_lag, .y = stops + n_ahead, ~seq(.x, .y))

  index <- list(
    train = train,
    test = test
  )

  return(index)

}



#' @title ...
#'
#' @description ...
#'
#' @param plan ...
#' @param context ...
#'
#' @return ...
#' @export

expand_split <- function(plan,
                         context) {

  series_id <- context[["series_id"]]

  plan <- plan %>%
    select(c(!!sym(series_id), n_splits, train, test)) %>%
    group_by(!!sym(series_id)) %>%
    mutate(split = list(1:n_splits)) %>%
    ungroup() %>%
    unnest(cols = c(split, train, test)) %>%
    select(c(!!sym(series_id), split, train, test))

  return(plan)
}



#' @title ...
#'
#' @description ...
#'
#' @param data ...
#' @param context ...
#' @param type ...
#' @param value ...
#' @param n_ahead ...
#' @param n_skip ...
#' @param n_lag ...
#' @param mode ...
#' @param exceed ...
#'
#' @return
#' @export

make_split <- function(main,
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
    main = main,
    context = context,
    type = type,
    value = value
  )

  # Create indices for train and test data and add as nested list
  split_frame <- map_dfr(
    .x = 1:nrow(split_frame),
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
      split_frame %>%
        slice(.x) %>%
        mutate(n_ahead = n_ahead) %>%
        mutate(n_splits = length(index$train)) %>%
        mutate(train = list(index$train)) %>%
        mutate(test = list(index$test))
    }
  )

  # Expand split_frame and return output
  split_frame <- expand_split(
    plan = split_frame,
    context = context
  )

  return(split_frame)
}

