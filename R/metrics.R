
#' @title Calculate the mean error.
#'
#' @description Calculate the mean error. `me()` is a metric that is in
#' the same units as the original data.
#'
#' @param data A `data.frame` containing the `truth` and `estimate`
#'    columns.
#' @param truth The column identifier for the true results
#'    (that is `numeric`). This should be an unquoted column name although
#'    this argument is passed by expression and supports
#'    [quasiquotation][rlang::quasiquotation] (you can unquote column
#'    names). For `_vec()` functions, a `numeric` vector.
#'
#' @param estimate The column identifier for the predicted
#'    results (that is also `numeric`). As with `truth` this can be
#'    specified different ways but the primary method is to use an
#'    unquoted variable name. For `_vec()` functions, a `numeric` vector.
#' @param na_rm A  `logical` value indicating whether `NA`
#'    values should be stripped before the computation proceeds.
#'
#' @param ... Not currently used.
#'
#' @return
#' @export

me <- function(data, ...) {
  UseMethod("me")
}

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



#' @title Calculate the mean squared error.
#'
#' @description Calculate the mean squared error. `mse()` is a metric that is in
#'    quadratic units.
#'
#' @param data A `data.frame` containing the `truth` and `estimate`
#'    columns.
#' @param truth The column identifier for the true results
#'    (that is `numeric`). This should be an unquoted column name although
#'    this argument is passed by expression and supports
#'    [quasiquotation][rlang::quasiquotation] (you can unquote column
#'    names). For `_vec()` functions, a `numeric` vector.
#'
#' @param estimate The column identifier for the predicted
#'    results (that is also `numeric`). As with `truth` this can be
#'    specified different ways but the primary method is to use an
#'    unquoted variable name. For `_vec()` functions, a `numeric` vector.
#' @param na_rm A  `logical` value indicating whether `NA`
#'    values should be stripped before the computation proceeds.
#'
#' @param ... Not currently used.
#'
#' @return
#' @export

mse <- function(data, ...) {
  UseMethod("mse")
}

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


#' @title Calculate the mean percentage error.
#'
#' @description Calculate the mean percentage error. `mpe()` is in relative units.
#'
#' @param data A `data.frame` containing the `truth` and `estimate`
#'    columns.
#' @param truth The column identifier for the true results
#'    (that is `numeric`). This should be an unquoted column name although
#'    this argument is passed by expression and supports
#'    [quasiquotation][rlang::quasiquotation] (you can unquote column
#'    names). For `_vec()` functions, a `numeric` vector.
#'
#' @param estimate The column identifier for the predicted
#'    results (that is also `numeric`). As with `truth` this can be
#'    specified different ways but the primary method is to use an
#'    unquoted variable name. For `_vec()` functions, a `numeric` vector.
#' @param na_rm A  `logical` value indicating whether `NA`
#'    values should be stripped before the computation proceeds.
#'
#' @param ... Not currently used.
#'
#' @return
#' @export

mpe <- function(data, ...) {
  UseMethod("mpe")
}

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
