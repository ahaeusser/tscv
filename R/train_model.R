
#' @title Train univariate models.
#'
#' @description This function trains several univariate models to the key variables within data (.variable and .slice).
#'    The user can define the models via \code{methods}. Currently, the following methods are available:
#'    \itemize{
#'       \item{sNaive: Set each forecast to be equal to the last observed value from the same season of the previous period.}
#'       \item{...}
#'       }
#'
#' @param data A tsibble in long format with standard columns (.date, .variable, .value, etc.).
#'
#' @return models A model object of class \code{mdl_df} ("mable").
#' @export

train_model <- function(data) {

  models <- data %>%
    filter(sample == "train") %>%
    model(
      sNaive = SNAIVE(value))
      # Prophet = prophet(value ~ season()),
      # FASSTER = FASSTER(value ~ poly(1) + trig(24, 6)))

  return(models)
}
