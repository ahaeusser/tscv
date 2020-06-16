
#' @title Calculate the mean error
#'
#' @description Calculate the mean error. \code{me()} is a metric that is in
#'    the same units as the original data.
#'
#' @param data A \code{data.frame}, \code{tibble} or \code{tsibble} containing
#'    the \code{truth} and \code{estimate}.
#' @param ... Not currently used.
#'
#' @return
#' @export

me <- function(data, ...) {
  UseMethod("me")
}


#' @title Calculate the mean error
#'
#' @description Calculate the mean error of a numeric vector. \code{me()}
#'    is a metric that is in the same units as the original data.
#'
#' @param truth Numeric vector containing the actual values.
#' @param estimate Numeric vector containing the forecasts.
#' @param na_rm Logical value. If \code{TRUE}, missing values are removed.
#' @param ... Further arguments passed to \code{metric_vec_template()}.
#'
#' @return Numeric value.

me_vec <- function(truth,
                   estimate,
                   na_rm = TRUE,
                   ...) {

  me_impl <- function(truth, estimate) {
    mean(truth - estimate)
  }

  metric_vec_template(
    metric_impl = me_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
}


#' @title Calculate the mean error
#'
#' @description Calculate the mean error for the columns of a data.frame.
#'    \code{me()} is a metric that is in the same units as the original data.
#'
#' @param data A \code{data.frame}, \code{tibble} or \code{tsibble} containing
#'    the \code{truth} and \code{estimate}.
#' @param truth Numeric vector containing the actual values.
#' @param estimate Numeric vector containing the forecasts.
#' @param na_rm Logical value. If \code{TRUE}, missing values are removed.
#' @param ... Further arguments passed to \code{metric_summarizer()}.
#'
#' @return A \code{data.frame} or \code{tibble}.

me.data.frame <- function(data,
                          truth,
                          estimate,
                          na_rm = TRUE,
                          ...) {
  metric_summarizer(
    metric_nm = "me",
    metric_fn = me_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    ...
  )
}



#' @title Calculate the mean squared error
#'
#' @description Calculate the mean squared error. \code{mse()} is a metric
#'    that is in quadratic units.
#'
#' @param data A \code{data.frame}, \code{tibble} or \code{tsibble} containing
#'    the \code{truth} and \code{estimate}.
#' @param ... Not currently used.
#'
#' @return
#' @export

mse <- function(data, ...) {
  UseMethod("mse")
}


#' @title Calculate the mean squared error
#'
#' @description Calculate the mean squared error of a numeric vector.
#'    \code{mse()} is a metric that is in quadratic units.
#'
#' @param truth Numeric vector containing the actual values.
#' @param estimate Numeric vector containing the forecasts.
#' @param na_rm Logical value. If \code{TRUE}, missing values are removed.
#' @param ... Further arguments passed to \code{metric_vec_template()}.
#'
#' @return Numeric value.

mse_vec <- function(truth,
                    estimate,
                    na_rm = TRUE,
                    ...) {

  mse_impl <- function(truth, estimate) {
    mean((truth - estimate) ^ 2)
  }

  metric_vec_template(
    metric_impl = mse_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
}


#' @title Calculate the mean squared error
#'
#' @description Calculate the mean squared error for the columns of a data.frame.
#'    \code{mse()} is a metric that is in quadratic units.
#'
#' @param data A \code{data.frame}, \code{tibble} or \code{tsibble} containing
#'    the \code{truth} and \code{estimate}.
#' @param truth Numeric vector containing the actual values.
#' @param estimate Numeric vector containing the forecasts.
#' @param na_rm Logical value. If \code{TRUE}, missing values are removed.
#' @param ... Further arguments passed to \code{metric_summarizer()}.
#'
#' @return A \code{data.frame} or \code{tibble}.

mse.data.frame <- function(data,
                           truth,
                           estimate,
                           na_rm = TRUE,
                           ...) {
  metric_summarizer(
    metric_nm = "mse",
    metric_fn = mse_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    ...
  )
}


#' @title Calculate the mean percentage error
#'
#' @description Calculate the mean percentage error. \code{mpe()} is a metric
#'    that is in relative units.
#'
#' @param data A \code{data.frame}, \code{tibble} or \code{tsibble} containing
#'    the \code{truth} and \code{estimate}.
#' @param ... Not currently used.
#'
#' @return
#' @export

mpe <- function(data, ...) {
  UseMethod("mpe")
}


#' @title Calculate the mean percentage error
#'
#' @description Calculate the mean percentage error of a numeric vector.
#'    \code{mpe()} is a metric that is in relative units.
#'
#' @param truth Numeric vector containing the actual values.
#' @param estimate Numeric vector containing the forecasts.
#' @param na_rm Logical value. If \code{TRUE}, missing values are removed.
#' @param ... Further arguments passed to \code{metric_vec_template()}.
#'
#' @return Numeric value.

mpe_vec <- function(truth,
                    estimate,
                    na_rm = TRUE,
                    ...) {

  mpe_impl <- function(truth, estimate) {
    mean(((truth - estimate) / truth) * 100)
  }

  metric_vec_template(
    metric_impl = mpe_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
}


#' @title Calculate the mean percentage error
#'
#' @description Calculate the mean percentage error for the columns of a data.frame.
#'    \code{mpe()} is a metric that is in relative units.
#'
#' @param data A \code{data.frame}, \code{tibble} or \code{tsibble} containing
#'    the \code{truth} and \code{estimate}.
#' @param truth Numeric vector containing the actual values.
#' @param estimate Numeric vector containing the forecasts.
#' @param na_rm Logical value. If \code{TRUE}, missing values are removed.
#' @param ... Further arguments passed to \code{metric_summarizer()}.
#'
#' @return A \code{data.frame} or \code{tibble}.

mpe.data.frame <- function(data,
                           truth,
                           estimate,
                           na_rm = TRUE,
                           ...) {
  metric_summarizer(
    metric_nm = "mpe",
    metric_fn = mpe_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    ...
  )
}
