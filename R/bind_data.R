
#' @title Bind data and forecasts row-wise into one tsibble.
#'
#' @description Bind data and forecasts row-wise into one tsibble. The key structure of both objects is updated.
#'
#' @param data A tsibble containing the data provide by \code{split_data(...)}.
#' @param fcsts A fable ('forecast table') provided by \code{forecast_model(...)}.
#'
#' @return data A tsibble containing the input data and the forecasts.
#' @export

bind_data <- function(data,
                      fcsts) {
  data <- data %>%
    update_tsibble(
      key = c(variable,
              slice,
              model,
              type))

  fcsts <- fcsts %>%
    update_tsibble(
      key = c(variable,
              slice,
              model,
              type))

  data <- rbind(data, fcsts)
  return(data)
}
