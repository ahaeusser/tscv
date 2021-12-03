
#' @title Seasonal mean model
#'
#' @description Train a seasonal mean model (SMEAN). This is equivalent to a
#'   linear regression against seasonal dummy variables only, i.e.
#'   \code{TSLM(value ~ season())}.
#'
#' @param .data Input data as tsibble.
#' @param specials Specials as list defined in \code{specials_smean}.
#' @param ... Currently not in use.
#'
#' @return An object of class \code{SMEAN}.

train_smean <- function(.data,
                        specials,
                        ...){

  if (length(measured_vars(.data)) > 1) {
    abort("Only univariate responses are supported by SMEAN.")
  }

  y <- unclass(.data)[[measured_vars(.data)]]

  if (all(is.na(y))) {
    abort("All observations are missing, a model cannot be estimated without data.")
  }

  # Prepare period
  lag <- specials$lag[[1]]
  n <- length(y)
  index <- rep(1:lag, times = ceiling(n / lag))[1:n]

  smean <- map_dbl(
    .x = 1:lag,
    .f = ~mean(y[index == .x], na.rm = TRUE)
  )

  fitted <- rep(smean, times = ceiling(n / lag))[1:n]
  resid <- y - fitted
  sigma <- sd(resid, na.rm = TRUE)

  structure(
    list(
      fitted = fitted,
      resid = resid,
      smean = smean,
      sigma = sigma,
      last_period = last(index),
      lag = lag),
    class = "SMEAN"
  )
}


specials_smean <- new_specials(
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


#' @title Seasonal mean model
#'
#' @description Automatic train a seasonal mean model (SMEAN). This is
#'   equivalent to a linear regression against seasonal dummy variables only,
#'   i.e. \code{TSLM(value ~ season())}.
#'
#' @param formula Model specification (see "Specials" section, currently not in use ...).
#' @param ... Further arguments.
#'
#' @return smean_model An object of class \code{SMEAN}.
#' @export

SMEAN <- function(formula, ...){
  smean_model <- new_model_class(
    model = "SMEAN",
    train = train_smean,
    specials = specials_smean)

  new_model_definition(
    smean_model,
    !!enquo(formula),
    ...)
}


#' @title Forecast a trained seasonal mean model
#'
#' @description Forecast a trained seasonal mean model.
#'
#' @param object An object of class \code{SMEAN}.
#' @param new_data Forecast horizon (n-step ahead forecast)
#' @param specials Specials are currently not in use.
#' @param ... Additional arguments for forecast method.
#'
#' @return An object of class \code{fable}.
#' @export

forecast.SMEAN <- function(object,
                           new_data,
                           specials = NULL,
                           ...){
  # Extract model
  smean <- object$smean
  last_period <- object$last_period

  n_ahead <- nrow(new_data)
  index <- rep(1:length(smean), times = ceiling((n_ahead + last_period) / length(smean)))[(last_period + 1):(last_period + n_ahead)]
  point <- as.numeric(smean[index])
  sd <- as.numeric(rep(object$sigma, times = n_ahead))

  # Return forecasts
  dist_normal(point, sd)
}


#' @title Extract fitted values from a trained seasonal mean model
#'
#' @description Extract fitted values from a trained seasonal mean model.
#'
#' @param object An object of class \code{SMEAN}.
#' @param ... Currently not in use.
#'
#' @return Fitted values as tsibble.
#' @export

fitted.SMEAN <- function(object, ...){
  object[["fitted"]]
}


#' @title Extract residuals from a trained seasonal mean model
#'
#' @description Extract residuals from a trained seasonal mean model.
#'
#' @param object An object of class \code{SMEAN}.
#' @param ... Currently not in use.
#'
#' @return Fitted values as tsibble.
#' @export

residuals.SMEAN <- function(object, ...){
  object[["resid"]]
}


#' @title Provide a succinct summary of a trained seasonal mean model
#'
#' @description Provide a succinct summary of a trained seasonal mean model.
#'
#' @param object An object of class \code{SMEAN}.
#'
#' @return Model summary as character value.
#' @export

model_sum.SMEAN <- function(object){
  "SMEAN"
}
