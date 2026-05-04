
#' @title Calculate the mean error
#'
#' @description Calculate the mean error of a numeric vector.
#'
#' @details
#' \code{me_vec()} computes the average signed forecast error
#' \code{truth - estimate}. Positive values indicate that forecasts are, on
#' average, below the observed values. Negative values indicate that forecasts
#' are, on average, above the observed values.
#'
#' The metric is reported in the same units as the original data.
#'
#' @param truth Numeric vector containing the actual values.
#' @param estimate Numeric vector containing the forecasts.
#' @param na_rm Logical value. If \code{TRUE}, missing values are removed.
#'
#' @return
#' A numeric value.
#'
#' @family accuracy functions
#' @export
#'
#' @examples
#' truth <- c(10, 20, 30)
#' estimate <- c(8, 22, 25)
#'
#' me_vec(truth, estimate)
#'
#' truth_na <- c(10, 20, NA)
#' estimate_na <- c(8, 22, 25)
#' me_vec(truth_na, estimate_na)

me_vec <- function(truth,
                   estimate,
                   na_rm = TRUE) {

  mean(truth - estimate, na.rm = na_rm)
}



#' @title Calculate the mean absolute error
#'
#' @description Calculate the mean absolute error of a numeric vector.
#'
#' @details
#' \code{mae_vec()} computes the average absolute forecast error
#' \code{abs(truth - estimate)}. The metric is reported in the same units as the
#' original data.
#'
#' @param truth Numeric vector containing the actual values.
#' @param estimate Numeric vector containing the forecasts.
#' @param na_rm Logical value. If \code{TRUE}, missing values are removed.
#'
#' @return
#' A numeric value.
#'
#' @family accuracy functions
#' @export
#'
#' @examples
#' truth <- c(10, 20, 30)
#' estimate <- c(8, 22, 25)
#'
#' mae_vec(truth, estimate)
#'
#' truth_na <- c(10, 20, NA)
#' estimate_na <- c(8, 22, 25)
#' mae_vec(truth_na, estimate_na)

mae_vec <- function(truth,
                    estimate,
                    na_rm = TRUE) {

  mean(abs(truth - estimate), na.rm = na_rm)
}



#' @title Calculate the mean squared error
#'
#' @description Calculate the mean squared error of a numeric vector.
#'
#' @details
#' \code{mse_vec()} computes the average squared forecast error
#' \code{(truth - estimate)^2}. The metric is reported in squared units of the
#' original data.
#'
#' @param truth Numeric vector containing the actual values.
#' @param estimate Numeric vector containing the forecasts.
#' @param na_rm Logical value. If \code{TRUE}, missing values are removed.
#'
#' @return
#' A numeric value.
#'
#' @family accuracy functions
#' @export
#'
#' @examples
#' truth <- c(10, 20, 30)
#' estimate <- c(8, 22, 25)
#'
#' mse_vec(truth, estimate)
#'
#' truth_na <- c(10, 20, NA)
#' estimate_na <- c(8, 22, 25)
#' mse_vec(truth_na, estimate_na)

mse_vec <- function(truth,
                    estimate,
                    na_rm = TRUE) {

  mean((truth - estimate) ^ 2, na.rm = na_rm)
}



#' @title Calculate the root mean squared error
#'
#' @description Calculate the root mean squared error of a numeric vector.
#'
#' @details
#' \code{rmse_vec()} computes the square root of the mean squared forecast error.
#' The metric is reported in the same units as the original data.
#'
#' @param truth Numeric vector containing the actual values.
#' @param estimate Numeric vector containing the forecasts.
#' @param na_rm Logical value. If \code{TRUE}, missing values are removed.
#'
#' @return
#' A numeric value.
#'
#' @family accuracy functions
#' @export
#'
#' @examples
#' truth <- c(10, 20, 30)
#' estimate <- c(8, 22, 25)
#'
#' rmse_vec(truth, estimate)
#'
#' truth_na <- c(10, 20, NA)
#' estimate_na <- c(8, 22, 25)
#' rmse_vec(truth_na, estimate_na)

rmse_vec <- function(truth,
                     estimate,
                     na_rm = TRUE) {

  sqrt(mean((truth - estimate) ^ 2, na.rm = na_rm))
}



#' @title Calculate the mean percentage error
#'
#' @description Calculate the mean percentage error of a numeric vector.
#'
#' @details
#' \code{mpe_vec()} computes the average signed percentage forecast error:
#' \code{((truth - estimate) / truth) * 100}. Positive values indicate that
#' forecasts are, on average, below the observed values in percentage terms.
#' Negative values indicate that forecasts are, on average, above the observed
#' values.
#'
#' This metric is undefined when \code{truth} is zero and may return
#' \code{Inf}, \code{-Inf}, or \code{NaN} in such cases.
#'
#' @param truth Numeric vector containing the actual values.
#' @param estimate Numeric vector containing the forecasts.
#' @param na_rm Logical value. If \code{TRUE}, missing values are removed.
#'
#' @return
#' A numeric value.
#'
#' @family accuracy functions
#' @export
#'
#' @examples
#' truth <- c(10, 20, 40)
#' estimate <- c(8, 22, 30)
#'
#' mpe_vec(truth, estimate)
#'
#' truth_na <- c(10, 20, NA)
#' estimate_na <- c(8, 22, 25)
#' mpe_vec(truth_na, estimate_na)

mpe_vec <- function(truth,
                    estimate,
                    na_rm = TRUE) {

  mean(((truth - estimate) / truth) * 100, na.rm = na_rm)
}



#' @title Calculate the mean absolute percentage error
#'
#' @description Calculate the mean absolute percentage error of a numeric vector.
#'
#' @details
#' \code{mape_vec()} computes the average absolute percentage forecast error:
#' \code{abs((truth - estimate) / truth) * 100}.
#'
#' This metric is undefined when \code{truth} is zero and may return
#' \code{Inf} or \code{NaN} in such cases.
#'
#' @param truth Numeric vector containing the actual values.
#' @param estimate Numeric vector containing the forecasts.
#' @param na_rm Logical value. If \code{TRUE}, missing values are removed.
#'
#' @return
#' A numeric value.
#'
#' @family accuracy functions
#' @export
#'
#' @examples
#' truth <- c(10, 20, 40)
#' estimate <- c(8, 22, 30)
#'
#' mape_vec(truth, estimate)
#'
#' truth_na <- c(10, 20, NA)
#' estimate_na <- c(8, 22, 25)
#' mape_vec(truth_na, estimate_na)

mape_vec <- function(truth,
                     estimate,
                     na_rm = TRUE) {

  mean(abs((truth - estimate) / truth), na.rm = na_rm) * 100
}



#' @title Calculate the symmetric mean absolute percentage error
#'
#' @description Calculate the symmetric mean absolute percentage error of a numeric vector.
#'
#' @details
#' \code{smape_vec()} computes the symmetric mean absolute percentage error:
#' \code{abs(estimate - truth) / ((abs(truth) + abs(estimate)) / 2) * 100}.
#'
#' This metric is undefined when both \code{truth} and \code{estimate} are zero
#' and may return \code{NaN} in such cases.
#'
#' @param truth Numeric vector containing the actual values.
#' @param estimate Numeric vector containing the forecasts.
#' @param na_rm Logical value. If \code{TRUE}, missing values are removed.
#'
#' @return
#' A numeric value.
#'
#' @family accuracy functions
#' @export
#'
#' @examples
#' truth <- c(10, 20, 40)
#' estimate <- c(8, 22, 30)
#'
#' smape_vec(truth, estimate)
#'
#' truth_na <- c(10, 20, NA)
#' estimate_na <- c(8, 22, 25)
#' smape_vec(truth_na, estimate_na)

smape_vec <- function(truth,
                      estimate,
                      na_rm = TRUE) {

  percent_scale <- 100
  numer <- abs(estimate - truth)
  denom <- (abs(truth) + abs(estimate)) / 2
  mean(numer / denom, na.rm = na_rm) * percent_scale
}
