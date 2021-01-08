
#' @title Double Seasonal Holt-Winters model
#'
#' @description Train a Double Seasonal Holt-Winters model (DSHW).
#'
#' @param .data Input data as tsibble.
#' @param specials Specials as list defined in \code{specials_dshw}.
#' @param periods Integer vector. The periodicity of the time series (e.g. \code{periods = c(24, 168)} for hourly data).
#' @param ... Further arguments passed to \code{forecast::dshw()}.
#'
#' @return An object of class \code{DSHW}.

train_dshw <- function(.data,
                       specials,
                       periods,
                       ...){

  if(length(tsibble::measured_vars(.data)) > 1){
    abort("Only univariate responses are supported by DSHW.")
  }

  # Prepare data for modeling
  y <- unclass(.data)[[measured_vars(.data)]]
  model_data <- msts(data = y, seasonal.periods = periods)

  if(any(is.na(model_data))){
    abort("DSHW does not support missing values.")
  }

  # Train model
  mdl <- forecast::dshw(y = model_data, ...)

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
    class = "DSHW")
}


specials_dshw <- new_specials()


#' @title Automatic train a DSHW model
#'
#' @description Automatic train a Double Seasonal Holt-Winters model (DSHW). This function
#'    is a wrapper for \code{forecast::dshw()}.
#'
#' @param formula Model specification (see "Specials" section, currently not in use ...)
#' @param ... Further arguments passed to \code{forecast::dshw()}.
#'
#' @return dshw_model An object of class \code{DSHW}.
#' @export

DSHW <- function(formula, ...){
  dshw_model <- new_model_class(
    model = "DSHW",
    train = train_dshw,
    specials = specials_dshw)

  new_model_definition(
    dshw_model,
    !!enquo(formula),
    ...)
}


#' @title Forecast a trained DSHW model
#'
#' @description Forecast a trained DSHW model.
#'
#' @param object An object of class \code{DSHW}.
#' @param new_data Forecast horizon (n-step ahead forecast)
#' @param specials Specials are currently not in use.
#' @param ... Additional arguments for forecast method.
#'
#' @return An object of class \code{fable}.
#' @export

forecast.DSHW <- function(object,
                          new_data,
                          specials = NULL,
                          ...){
  # Forecast model
  fcst <- forecast(object$model, h = nrow(new_data))

  # Extract point forecast and simulations
  point <- as.numeric(fcst$mean)
  sd <- as.numeric(object$sigma)

  # Return forecasts
  dist_normal(point, sd)
}


#' @title Extract fitted values from a trained DSHW model
#'
#' @description Extract fitted values from a trained DSHW model.
#'
#' @param object An object of class \code{DSHW}.
#' @param ... Currently not in use.
#'
#' @return Fitted values as tsibble.
#' @export

fitted.DSHW <- function(object, ...){
  object$est[[".fitted"]]
}


#' @title Extract residuals from a trained DSHW model
#'
#' @description Extract residuals from a trained DSHW model.
#'
#' @param object An object of class \code{DSHW}.
#' @param ... Currently not in use.
#'
#' @return Residuals as tsibble.
#' @export

residuals.DSHW <- function(object, ...){
  object$est[[".resid"]]
}


#' @title Provide a succinct summary of a trained DSHW model
#'
#' @description Provide a succinct summary of a trained DSHW model.
#'
#' @param object An object of class \code{DSHW}.
#'
#' @return Model summary as character value.
#' @export

model_sum.DSHW <- function(object){
  "DSHW"
}
