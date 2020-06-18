
#' @title Calculate the mean error
#'
#' @description Calculate the mean error of a numeric vector. \code{me()}
#'    is a metric that is in the same units as the original data.
#'
#' @param truth Numeric vector containing the actual values.
#' @param estimate Numeric vector containing the forecasts.
#' @param na_rm Logical value. If \code{TRUE}, missing values are removed.
#'
#' @return Numeric value.

me_vec <- function(truth,
                   estimate,
                   na_rm = TRUE) {

  mean(truth - estimate, na.rm = na_rm)
}



#' @title Calculate the mean absolute error
#'
#' @description Calculate the mean absolute error of a numeric vector.
#'    \code{mae()} is a metric that is in the same units as the original data.
#'
#' @param truth Numeric vector containing the actual values.
#' @param estimate Numeric vector containing the forecasts.
#' @param na_rm Logical value. If \code{TRUE}, missing values are removed.
#'
#' @return Numeric value.

mae_vec <- function(truth,
                    estimate,
                    na_rm = TRUE) {

  mean(abs(truth - estimate), na.rm = na_rm)
}



#' @title Calculate the mean squared error
#'
#' @description Calculate the mean squared error of a numeric vector.
#'    \code{mse()} is a metric that is in quadratic units.
#'
#' @param truth Numeric vector containing the actual values.
#' @param estimate Numeric vector containing the forecasts.
#' @param na_rm Logical value. If \code{TRUE}, missing values are removed.
#'
#' @return Numeric value.

mse_vec <- function(truth,
                    estimate,
                    na_rm = TRUE) {

  mean((truth - estimate) ^ 2, na.rm = na_rm)
}



#' @title Calculate the root mean squared error
#'
#' @description Calculate the root mean squared error of a numeric vector.
#'    \code{rmse()} is a metric that is in the same units as the original data.
#'
#' @param truth Numeric vector containing the actual values.
#' @param estimate Numeric vector containing the forecasts.
#' @param na_rm Logical value. If \code{TRUE}, missing values are removed.
#'
#' @return Numeric value.

rmse_vec <- function(truth,
                     estimate,
                     na_rm = TRUE) {

  sqrt(mean((truth - estimate) ^ 2, na.rm = na_rm))
}



#' @title Calculate the mean percentage error
#'
#' @description Calculate the mean percentage error of a numeric vector.
#'    \code{mpe()} is a metric that is in relative units.
#'
#' @param truth Numeric vector containing the actual values.
#' @param estimate Numeric vector containing the forecasts.
#' @param na_rm Logical value. If \code{TRUE}, missing values are removed.
#'
#' @return Numeric value.


mpe_vec <- function(truth,
                    estimate,
                    na_rm = TRUE) {

  mean(((truth - estimate) / truth) * 100, na.rm = na_rm)
}



#' @title Calculate the mean absolute percentage error
#'
#' @description Calculate the mean absolute percentage error of a numeric vector.
#'    \code{mape()} is a metric that is in relative units.
#'
#' @param truth Numeric vector containing the actual values.
#' @param estimate Numeric vector containing the forecasts.
#' @param na_rm Logical value. If \code{TRUE}, missing values are removed.
#'
#' @return Numeric value.

mape_vec <- function(truth,
                     estimate,
                     na_rm = TRUE) {

  mean(abs((truth - estimate) / truth), na.rm = na_rm) * 100
}



#' @title Calculate the symmetric mean absolute percentage error
#'
#' @description Calculate the symmetric mean absolute percentage error
#'    of a numeric vector. \code{mape()} is a metric that is in relative units.
#'
#' @param truth Numeric vector containing the actual values.
#' @param estimate Numeric vector containing the forecasts.
#' @param na_rm Logical value. If \code{TRUE}, missing values are removed.
#'
#' @return Numeric value.

smape_vec <- function(truth,
                      estimate,
                      na_rm = TRUE) {

  percent_scale <- 100
  numer <- abs(estimate - truth)
  denom <- (abs(truth) + abs(estimate)) / 2
  mean(numer / denom, na.rm = na_rm) * percent_scale
}
