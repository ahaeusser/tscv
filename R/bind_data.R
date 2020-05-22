
#' @title Bind data and forecasts row-wise into one tsibble.
#'
#' @description Bind data and forecasts row-wise into one tsibble. The key structure of both objects is updated.
#'
#' @param data A tsibble containing the data provided by \code{split_data(...)}.
#' @param fcsts A fable ('forecast table') provided by \code{forecast_model(...)}.
#'
#' @return data A tsibble containing the input data and the forecasts.
#' @export

bind_data <- function(data,
                      fcsts) {

  date_time <- index_var(fcsts)
  variable <- key_vars(fcsts)

  fcsts <- fcsts %>%
    as_tibble() %>%
    group_by(!!!syms(variable)) %>%
    mutate(horizon = row_number()) %>%
    mutate(sample = "test") %>%
    mutate(type = "fcst") %>%
    ungroup() %>%
    rename(model = .model) %>%
    select(-.distribution)

  data <- data %>%
    as_tibble() %>%
    mutate(model = NA_character_) %>%
    mutate(type = "actual") %>%
    select(-id)

  data <- rbind(data, fcsts)

  variable[variable == ".model"] <- "model"

  data <- data %>%
    as_tsibble(
      index = !!sym(date_time),
      key = c(!!!syms(variable), type))

  return(data)
}
