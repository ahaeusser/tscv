
#' @title Return target variables.
#'
#' @description \code{target_vars()} returns a character vector with the target variables, i.e.
#'    key variables without "helper variables" like \code{.split}, \code{.id}, \code{.sample}, etc.
#'
#' @param .data A \code{tsibble} or \code{fable}.
#'
#' @return target A character vector.
#' @export

target_vars <- function(.data) {
  keys <- key_vars(.data)
  drop <- c("split", "id", "sample", "horizon", ".model", ".mean", ".distribution")
  target <- keys[!keys %in% drop]
  return(target)
}


#' @title Return value variable.
#'
#' @description \code{value_var()} returns a character with the value variable, i.e.
#'    measured variables without "helper variables" like \code{.split}, \code{.id}, \code{.sample}, etc.
#'
#' @param .data A \code{tsibble} or \code{fable}.
#'
#' @return value A character vector.
#' @export

value_var <- function(.data) {
  value <- measured_vars(.data)
  drop <- c("split", "id", "sample", "horizon", ".model", ".mean", ".distribution")
  value <- value[!value %in% drop]
  return(value)
}



#' @title Replace outliers based on IQR method by NAs.
#'
#' @description \code{iqr_vec} takes a numeric vector and replaces
#'    outliers based on the IQR method by NAs.
#'
#' @param x Numeric vector.
#' @param alpha Numeric value. Controls the width of the limits.
#'
#' @return x Numeric vector.

iqr_vec <- function(x,
                    alpha = 0.05) {

  # Estimate 25% and 75% quantiles
  quantile_x <- stats::quantile(x, prob = c(0.25, 0.75), na.rm = TRUE)

  # Calculate interquartile range and define lower and upper limit
  iqr <- quantile_x[[2]] - quantile_x[[1]]
  limits <- quantile_x + (0.15 / alpha) * iqr * c(-1, 1)

  # Identify the index outliers within the vector and replace values with NAs
  outlier_idx <- which((x < limits[1]) | (x > limits[2]))
  x[outlier_idx] <- NA_real_
  return(x)
}


#' @title Linear interpolation of NAs.
#'
#' @description \code{approx_vec} linearly interpolates NAs
#'    within a numeric vector.
#'
#' @param x Numeric vector.
#'
#' @return x Numeric vector.

approx_vec <- function(x) {

  # Index of missing and non-missing values, etc.
  missing <- is.na(x)
  n <- length(x)
  tt <- 1:n
  idx <- tt[!missing]

  # Use linear interpolation
  x <- approx(idx, x[idx], tt, rule = 2)$y
}



#' @title Measure of kurtosis.
#'
#' @param x Numeric vector.
#' @param na_rm Logical value. If \code{TRUE}, missing values are dropped.
#'
#' @return Numeric value.

kurtosis_vec <- function(x,
                         na_rm = TRUE) {

  if (na_rm == TRUE) {
    x <- x[!is.na(x)]
    }

  n <- length(x)
  n * sum((x - mean(x))^4) / (sum((x - mean(x))^2)^2)
}



#' @title Measure of skewness.
#'
#' @param x Numeric vector.
#' @param na_rm Logical value. If \code{TRUE}, missing values are dropped.
#'
#' @return Numeric value.

skewness_vec <- function(x,
                         na_rm = TRUE) {

  if (na_rm == TRUE) {
    x <- x[!is.na(x)]
  }

  n <- length(x)
  (sum((x - mean(x))^3) / n) / (sum((x - mean(x))^2) / n)^(3/2)
}

