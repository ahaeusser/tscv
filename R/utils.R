
#' @title Return target variables
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


#' @title Return value variable
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



#' @title Estimate mode of a distribution based on Kernel Density Estimation
#'
#' @description The function estimates the mode of a distribution based on Kernel Density Estimation.
#'
#' @param x Numeric vector.
#' @param ... Further arguments passed to \code{stats::densitiy()}.
#'
#' @return mode Numeric value. The mode of the distribution.
#' @export

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
#' @description The function estimates the kurtosis of a distribution.
#'
#' @param x Numeric vector.
#' @param na_rm Logical value. If \code{TRUE}, missing values are dropped.
#'
#' @return Numeric value.
#' @export

estimate_kurtosis <- function(x, na_rm = TRUE) {

  if (na_rm == TRUE) {
    x <- x[!is.na(x)]
    }

  n <- length(x)
  n * sum((x - mean(x))^4) / (sum((x - mean(x))^2)^2)
}



#' @title Estimate skewness
#'
#' @description The function estimates the skewness of a numeric vector.
#'
#' @param x Numeric vector.
#' @param na_rm Logical value. If \code{TRUE}, missing values are dropped.
#'
#' @return Numeric value.
#' @export

estimate_skewness <- function(x, na_rm = TRUE) {

  if (na_rm == TRUE) {
    x <- x[!is.na(x)]
  }

  n <- length(x)
  (sum((x - mean(x))^3) / n) / (sum((x - mean(x))^2) / n)^(3/2)
}



#' @title Interpolate missing values
#'
#' @description The function \code{interpolate_missing()} is a wrapper
#'   for \code{forecast::na.interp()}, working with numeric vectors. For
#'   non-seasonal time series, linear interpolation is used and for
#'   seasonal time series, the series is decomposed via STL and the
#'   seasonally adjusted series is linearly interpolated and the seasonal
#'   component is added back.
#'
#' @param x Numeric vector.
#' @param period Numeric vector. The seasonal periods of the time series.
#' @param ... Further arguments passed to \code{forecast::msts()} or \code{forecast::na.interp()}.
#'
#' @return Numeric vector.
#' @export

interpolate_missing <- function(x,
                                period,
                                ...) {
  # Create msts object
  x <- msts(data = x, seasonal.periods = period)

  # Interpolate missing values
  x <- forecast::na.interp(x = x, ...)

  return(x)
}



#' @title Identify and replace outliers
#'
#' @description The function \code{smooth_outlier()} is a wrapper
#'   for \code{forecast::tsoutliers()}, working with numeric vectors. For
#'   non-seasonal time series, the supsmu method is used. For seasonal
#'   time series, the series is decomposed via STL and the IQR method is
#'   used on the remainder component. Values outside the range are
#'   linear interpolated on the remainder and the series is reconstructed
#'   with the corrected remainder component.
#'
#' @param x Numeric vector.
#' @param period Numeric vector. The seasonal periods of the time series.
#' @param ... Further arguments passed to \code{forecast::msts()} or \code{forecast::tsoutliers()}.
#'
#' @return Numeric vector.
#' @export

smooth_outlier <- function(x,
                           period,
                           ...) {
  # Create msts object
  x <- msts(data = x, seasonal.periods = period)

  # Identify outliers
  xs <- forecast::tsoutliers(x = x, ...)

  # Replace outliers
  x[xs$index] <- xs$replacements

  return(x)
}




#' @title Assign objects within a list to an environment
#'
#' @description \code{lst_to_env} is a helper function that assigns
#'   the objects within a list to an environment.
#'
#' @param x A list containing the objects to assign.
#' @param envir The environment to use (default is \code{.GlobalEnv}).
#' @param ... Further arguments passed to \code{assign()}.

#' @export

lst_to_env <- function(x,
                       envir = .GlobalEnv,
                       ...) {

  lst_names <- names(x)

  for (i in lst_names) {
    assign(
      x = i,
      value = x[[i]],
      envir = envir,
      ...
    )
  }
}



#' @title Negated value matching
#'
#' @description The function '%out%' is the negation of '%in%'.
#'
#' @param x Values to be matched.
#' @param table Values to be matched against.
#'
#' @return Logical vector, indicating if a non-match was located for each element of x
#' @export

'%out%' <- function(x,
                    table) {
  match(x, table, nomatch = 0L) == 0L
}




#' @title Create a name for a folder or file
#'
#' @description The function creates a name for a folder or file.
#'
#' @param primary Character value. Primary file name to be used.
#' @param extension Character value. File name extension to be used
#'   (e.g. \code{".csv"}, \code{".rda"}).
#' @param add_time Logical value. If \code{TRUE}, a time stamp is added in
#'   front of the file name.
#' @param time_format Character value. The time format of the time stamp.
#'
#' @return Character value.
#' @export

create_name <- function(primary,
                        extension = NULL,
                        add_time = TRUE,
                        time_format = "%Y%m%d_%H%M%S") {

  if (add_time == TRUE) {
    time_stamp <- format(Sys.time(), format = time_format)
    time_stamp <- paste0(time_stamp, "_")
  } else {
    time_stamp <- NULL
  }

  paste0(time_stamp, primary, extension)
}
