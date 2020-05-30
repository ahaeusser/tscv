
#' @title Train seasonal mean model.
#'
#' @description Train seasonal mean model. This is equivalent to a linear regression
#'    against seasonal dummy variables only (\code{TSLM(value ~ season())}).
#'
#' @param data Input data as \code{tsibble}.
#' @param specials Specials as list defined in \code{specials_smean}.
#' @param period Integer value. The seasonal period for the calculation.
#' @param ... Further arguments.
#'
#' @return An object of class \code{SMEAN}.

train_smean <- function(.data,
                        specials,
                        period,
                        ...){

  if (length(measured_vars(.data)) > 1) {
    abort("Only univariate responses are supported by SMEAN.")
  }

  y <- unclass(.data)[[measured_vars(.data)]]

  if (all(is.na(y))) {
    abort("All observations are missing, a model cannot be estimated without data.")
  }

  # Prepare period
  n <- length(y)
  index <- rep(1:period, times = ceiling(n / period))[1:n]

  means <- map_dbl(
    .x = 1:period,
    .f = ~mean(y[index == .x], na.rm = TRUE)
  )

  fit <- rep(means, times = ceiling(n / period))[1:n]
  res <- y - fit
  sigma <- sd(res, na.rm = TRUE)

  structure(
    list(
      .fitted = fit,
      .resid = res,
      mean = means,
      sigma = sigma,
      last_period = last(index),
      period = period
    ),
    class = "SMEAN"
  )
}



specials_smean <- new_specials()


#' @title Automatic training of SMEANs.
#'
#' @description Automatic training of SMEANs.
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


#' @title Forecast a trained SMEAN model.
#'
#' @description Forecast a trained SMEAN model.
#'
#' @param object Trained model.
#' @param new_data Forecast horizon.
#' @param specials Specials are currently not in use.
#' @param ... Further arguments.
#'
#' @return A \code{fable}.
#' @export

forecast.SMEAN <- function(object,
                           new_data,
                           specials = NULL,
                           ...){
  # Extract model
  means <- object$mean
  last_period <- object$last_period
  sigma <- object$sigma

  n_ahead <- nrow(new_data)
  index <- rep(1:length(means), times = ceiling((n_ahead + last_period) / length(means)))[(last_period + 1):(last_period + n_ahead)]
  fcst <- means[index]

  # Return forecasts
  construct_fc(
    point = fcst,
    sd = rep(sigma, times = n_ahead),
    dist = dist_normal(mean = fcst, sd = rep(sigma, times = n_ahead)))
}


#' @title Extract fitted values from SMEAN.
#'
#' @description Extract fitted values from SMEAN.
#'
#' @param object The time series model used to produce the forecast.
#' @param ... Further arguments.
#'
#' @return
#' @export

fitted.SMEAN <- function(object, ...){
  object$est[[".fitted"]]
}


#' @title Extract residuals from SMEAN.
#'
#' @description Extract residuals from SMEAN.
#'
#' @param object The time series model used to produce the forecast.
#' @param ... Further arguments.
#'
#' @return
#' @export

residuals.SMEAN <- function(object, ...){
  object$est[[".resid"]]
}

#' @title Provide a succinct summary of the SMEAN.
#'
#' @description Provide a succinct summary of the SMEAN.
#'
#' @param object The SMEAN to summarize.
#'
#' @return
#' @export

model_sum.SMEAN <- function(x){
  "SMEAN"
}
