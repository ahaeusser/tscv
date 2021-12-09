
#' @title Estimate mode of a distribution based on Kernel Density Estimation
#'
#' @description The function estimates the mode of a distribution based on Kernel Density Estimation.
#'
#' @param x Numeric vector.
#' @param na_rm Logical value. If \code{TRUE}, missing values are dropped.
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

file_name <- function(primary,
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



#' @title Helper function to create numbered strings.
#'
#' @description Creates a character vector in the form
#'   \code{c("x(1)", x(2), ..., x(n))}.
#'
#' @param x Character value.
#' @param n Integer value.
#'
#' @return x Character vector.
#' @export

number_string <- function(x, n) {
  x <- paste0(
    x,"(",
    formatC(
      x = 1:n,
      width = nchar(n),
      flag = "0"),
    ")")

  return(x)
}



#' @title Create string with elapsed time.
#'
#' @description Create a string with the elapsed time between start and end for
#'   the log file.
#'
#' @param start Start date and time.
#' @param end End date and time.
#' @param digits Integer value. The number of digits for rounding.
#'
#' @return Character value.
#' @export

log_time <- function(start = Sys.time(),
                     end = Sys.time(),
                     digits = 1) {
  diff <- end - start
  time <- round(diff[[1]], digits = digits)
  unit <- units(diff)
  x <- glue("TIME  [{time} {unit}]")
  return(x)
}



#' @title Create string with header for log file.
#'
#' @description Create a string with information about the current R session
#'   for the log file.
#'
#' @return Character value.
#' @export

log_header <- function() {

  session <- session_info()$platform
  user <- Sys.info()[["user"]]
  rstudio <- versionInfo()

  x <- glue(
    "-------------------------------------- \n",
    "version: {session$version} \n",
    "os:      {session$os} \n",
    "system:  {session$system} \n",
    "ui:      {session$ui} {rstudio$version} \n",
    "user:    {user} \n",
    "date:    {session$date} \n",
    "tz:      {session$tz} \n",
    "-------------------------------------- \n"
  )
  return(x)
}
