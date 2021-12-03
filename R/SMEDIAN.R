
#' @title Seasonal median model
#'
#' @description Train seasonal median model (SMEDIAN).
#'
#' @param .data Input data as tsibble.
#' @param specials Specials as list defined in \code{specials_smedian}.
#' @param ... Currently not in use.
#'
#' @return An object of class \code{SMEDIAN}.

train_smedian <- function(.data,
                          specials,
                          ...){

  if (length(measured_vars(.data)) > 1) {
    abort("Only univariate responses are supported by SMEDIAN.")
  }

  y <- unclass(.data)[[measured_vars(.data)]]

  if (all(is.na(y))) {
    abort("All observations are missing, a model cannot be estimated without data.")
  }

  # Prepare period
  lag <- specials$lag[[1]]
  n <- length(y)
  index <- rep(1:lag, times = ceiling(n / lag))[1:n]

  smedian <- map_dbl(
    .x = 1:lag,
    .f = ~median(y[index == .x], na.rm = TRUE)
  )

  fitted <- rep(smedian, times = ceiling(n / lag))[1:n]
  resid <- y - fitted
  sigma <- sd(resid, na.rm = TRUE)

  structure(
    list(
      fitted = fitted,
      resid = resid,
      smedian = smedian,
      sigma = sigma,
      last_period = last(index),
      lag = lag),
    class = "SMEDIAN"
  )
}


globalVariables(c("self", "origin"))

specials_smedian <- new_specials(
  lag = function(lag = NULL) {
    lag <- get_frequencies(period = lag, data = self$data, .auto = "smallest")
    if (lag == 1) {
      abort("Non-seasonal model specification provided, use RW() or provide a different lag specification.")
    }
    if (!rlang::is_integerish(lag)) {
      warn("Non-integer lag orders for random walk models are not supported. Rounding to the nearest integer.")
      lag <- round(lag)
    }
    lag
  },
  .required_specials = c("lag")
)


#' @title Seasonal median model
#'
#' @description Automatic train a seasonal median model (SMEDIAN).
#'
#' @param formula Model specification (see "Specials" section, currently not in use ...).
#' @param ... Further arguments.
#'
#' @return smedian_model An object of class \code{SMEDIAN}.
#' @export

SMEDIAN <- function(formula, ...){
  smedian_model <- new_model_class(
    model = "SMEDIAN",
    train = train_smedian,
    specials = specials_smedian)

  new_model_definition(
    smedian_model,
    !!enquo(formula),
    ...)
}


#' @title Forecast a trained seasonal median model
#'
#' @description Forecast a trained seasonal median model.
#'
#' @param object An object of class \code{SMEDIAN}.
#' @param new_data Forecast horizon (n-step ahead forecast)
#' @param specials Specials are currently not in use.
#' @param ... Additional arguments for forecast method.
#'
#' @return An object of class \code{fable}.
#' @export

forecast.SMEDIAN <- function(object,
                             new_data,
                             specials = NULL,
                             ...){
  # Extract model
  smedian <- object$smedian
  last_period <- object$last_period

  n_ahead <- nrow(new_data)
  index <- rep(1:length(smedian), times = ceiling((n_ahead + last_period) / length(smedian)))[(last_period + 1):(last_period + n_ahead)]
  point <- as.numeric(smedian[index])
  sd <- as.numeric(rep(object$sigma, times = n_ahead))

  # Return forecasts
  dist_normal(point, sd)

}


#' @title Extract fitted values from a trained seasonal median model
#'
#' @description Extract fitted values from a trained seasonal median model.
#'
#' @param object An object of class \code{SMEDIAN}.
#' @param ... Currently not in use.
#'
#' @return Fitted values as tsibble.
#' @export

fitted.SMEDIAN <- function(object, ...){
  object[["fitted"]]
}


#' @title Extract residuals from a trained seasonal median model
#'
#' @description Extract residuals from a trained seasonal median model.
#'
#' @param object An object of class \code{SMEDIAN}.
#' @param ... Currently not in use.
#'
#' @return Fitted values as tsibble.
#' @export

residuals.SMEDIAN <- function(object, ...){
  object[["resid"]]
}


#' @title Provide a succinct summary of a trained seasonal median model
#'
#' @description Provide a succinct summary of a trained seasonal median model.
#'
#' @param object An object of class \code{SMEDIAN}.
#'
#' @return Model summary as character value.
#' @export

model_sum.SMEDIAN <- function(object){
  "SMEDIAN"
}
