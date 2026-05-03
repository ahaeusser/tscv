
#' @title Estimate the mode of a distribution
#'
#' @description
#' Estimate the mode of a numeric distribution using kernel density estimation.
#'
#' @details
#' The function computes a kernel density estimate with \code{stats::density()}
#' and returns the value of \code{x} at which the estimated density is largest.
#'
#' Missing values are removed by default. Additional arguments are passed to
#' \code{stats::density()}, for example \code{bw}, \code{kernel}, or \code{n}.
#'
#' @param x Numeric vector.
#' @param na_rm Logical value. If \code{TRUE}, missing values are removed before
#'   estimation.
#' @param ... Further arguments passed to \code{stats::density()}.
#'
#' @return
#' A numeric value giving the estimated mode of the distribution.
#'
#' @family Miscellaneous
#' @export
#'
#' @examples
#' x <- c(1, 1, 2, 2, 2, 3, 4, NA)
#'
#' estimate_mode(x)
#' estimate_mode(x, na_rm = TRUE)
#' estimate_mode(x, bw = "nrd0")
#'
#' set.seed(123)
#' y <- rnorm(100, mean = 5)
#' estimate_mode(y)

estimate_mode <- function(x,
                          na_rm = TRUE,
                          ...) {

  if (na_rm == TRUE) {
    x <- x[!is.na(x)]
  }

  object <- density(x = x, ...)
  mode_id <- which.max(object$y)
  mode <- object$x[mode_id]

  return(mode)
}



#' @title Estimate kurtosis
#'
#' @description Estimate the kurtosis of a numeric distribution.
#'
#' @details The function computes the moment-based kurtosis
#'
#' \deqn{
#'   \frac{n \sum_i (x_i - \bar{x})^4}
#'        {\left(\sum_i (x_i - \bar{x})^2\right)^2}
#' }
#'
#' Missing values are removed by default.
#'
#' This returns the usual kurtosis, not excess kurtosis. A normal distribution
#' has kurtosis close to \code{3}.
#'
#' @param x Numeric vector.
#' @param na_rm Logical value. If \code{TRUE}, missing values are removed before
#'   estimation.
#'
#' @return A numeric value giving the estimated kurtosis.
#'
#' @family Miscellaneous
#' @export
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5, NA)
#'
#' estimate_kurtosis(x)
#' estimate_kurtosis(x, na_rm = TRUE)
#'
#' set.seed(123)
#' y <- rnorm(100)
#' estimate_kurtosis(y)

estimate_kurtosis <- function(x, na_rm = TRUE) {

  if (na_rm == TRUE) {
    x <- x[!is.na(x)]
  }

  n <- length(x)
  n * sum((x - mean(x))^4) / (sum((x - mean(x))^2)^2)
}



#' @title Estimate skewness
#'
#' @description Estimate the skewness of a numeric distribution.
#'
#' @details
#' The function computes the moment-based skewness
#'
#' \deqn{
#'   \frac{\frac{1}{n}\sum_i (x_i - \bar{x})^3}
#'        {\left(\frac{1}{n}\sum_i (x_i - \bar{x})^2\right)^{3/2}}
#' }
#'
#' Missing values are removed by default. Positive values indicate a distribution
#' with a longer or heavier right tail; negative values indicate a distribution
#' with a longer or heavier left tail.
#'
#' @param x Numeric vector.
#' @param na_rm Logical value. If \code{TRUE}, missing values are removed before
#'   estimation.
#'
#' @return A numeric value giving the estimated skewness.
#'
#' @family Miscellaneous
#' @export
#'
#' @examples
#' x <- c(1, 2, 3, 4, 10, NA)
#'
#' estimate_skewness(x)
#' estimate_skewness(x, na_rm = TRUE)
#'
#' set.seed(123)
#' y <- rexp(100)
#' estimate_skewness(y)

estimate_skewness <- function(x, na_rm = TRUE) {

  if (na_rm == TRUE) {
    x <- x[!is.na(x)]
  }

  n <- length(x)
  (sum((x - mean(x))^3) / n) / (sum((x - mean(x))^2) / n)^(3/2)
}
