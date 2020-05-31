
#' @title Return response variables.
#'
#' @description \code{response_vars()} returns a character vector with the response variables, i.e.
#'    key variables without "helper variables" like \code{split}, \code{id}, \code{sample}, etc.
#'
#' @param .data A \code{tsibble} or \code{fable}.
#'
#' @return response A character vector.
#' @export

response_vars <- function(.data) {
  keys <- key_vars(.data)
  drop <- c("split", "id", "sample", "horizon", ".model")
  response <- keys[!keys %in% drop]
  return(response)
}


#' @title Return value variable.
#'
#' @description \code{value_var()} returns a character with the value variable, i.e.
#'    measured variables without "helper variables" like \code{split}, \code{id}, \code{sample}, etc.
#'
#' @param .data A \code{tsibble} or \code{fable}.
#'
#' @return value A character vector.
#' @export

value_var <- function(.data) {
  value <- measured_vars(.data)
  drop <- c("split", "id", "sample", "horizon", ".model", ".distribution")
  value <- value[!value %in% drop]
  return(value)
}
