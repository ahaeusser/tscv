
#' @title Train THETA model.
#'
#' @description Train a THETA model.
#'
#' @param .data Input data as \code{tsibble}.
#' @param specials Specials as list defined in \code{specials_thetaf}.
#' @param ... Further arguments passed to \code{forecast::thetaf()}.
#'
#' @return An object of class \code{THETA}.

train_theta <- function(.data,
                        specials,
                        ...){

  if(length(tsibble::measured_vars(.data)) > 1){
    abort("Only univariate responses are supported by THETA.")
  }

  # Prepare data for modelling
  y <- unclass(.data)[[measured_vars(.data)]]
  model_data <- as.ts(.data)

  if(any(is.na(model_data))){
    abort("THETA does not support missing values.")
  }

  # Train model
  mdl <- forecast::thetaf(y = model_data, ...)

  # Extract fitted values and residuals
  fitted <- mdl$fitted
  resid <- mdl$residuals
  sigma <- sd(resid, na.rm = TRUE)

  # Return model
  structure(
    list(
      model = mdl,
      est = list(
        .fitted = fitted,
        .resid = resid),
      sigma = sigma),
    class = "THETA")
}



specials_theta <- new_specials()

#' @title Automatic training of THETA.
#'
#' @description Automatic training a THETA model. This function
#'    is a wrapper for \code{forecast::thetaf()}.
#'
#' @param formula Model specification (see "Specials" section, currently not in use...)
#' @param ... Further arguments passed to \code{forecast::theta()}.
#'
#' @return theta_model An object of class \code{THETA}.
#' @export

THETA <- function(formula, ...){
  theta_model <- new_model_class(
    model = "THETA",
    train = train_theta,
    specials = specials_theta)

  new_model_definition(
    theta_model,
    !!enquo(formula),
    ...)
}


#' @title Forecast a trained THETA model.
#'
#' @description Forecast a trained THETA model.
#'
#' @param object The trained THETA model used to produce the forecasts.
#' @param new_data Forecast horizon.
#' @param specials Specials are currently not in use.
#' @param ... Additional arguments for forecast model method.
#'
#' @return An object of class "fable".
#' @export

forecast.THETA <- function(object,
                         new_data,
                         specials = NULL,
                         ...){
  # Forecast model
  fcst <- forecast(object$model, h = nrow(new_data))

  # Extract point forecast and simulations
  mean <- fcst$mean
  sigma <- object$sigma

  # Return forecasts
  construct_fc(
    point = mean,
    sd = sigma,
    dist = dist_normal(mean = mean, sd =  rep(sigma, times = n_ahead)))
}


#' @title Extract fitted values from a THETA.
#'
#' @description Extract fitted values from a THETA.
#'
#' @param object The time series model used to produce the forecast.
#' @param ... Further arguments.
#'
#' @return
#' @export

fitted.THETA <- function(object, ...){
  object$est[[".fitted"]]
}


#' @title Extract residuals from a THETA.
#'
#' @description Extract residuals from a THETA.
#'
#' @param object The time series model used to produce the forecast.
#' @param ... Further arguments.
#'
#' @return
#' @export

residuals.THETA <- function(object, ...){
  object$est[[".resid"]]
}

#' @title Provide a succinct summary of the THETA.
#'
#' @description Provide a succinct summary of the THETA.
#'
#' @param object The THETA to summarize.
#'
#' @return
#' @export

model_sum.THETA <- function(x){
  "THETA"
}
