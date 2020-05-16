
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

  variable <- key_vars(data)

  data <- data %>%
    update_tsibble(
      key = c(
        !!!syms(variable),
        model,
        type))

  fcsts <- fcsts %>%
    update_tsibble(
      key = c(
        !!!syms(variable),
        model,
        type))

  data <- rbind(data, fcsts)
  return(data)
}
