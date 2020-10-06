
#' @title Train TBATS model
#'
#' @description Train a TBATS model (Trigonometric seasonality, Box-Cox transformation,
#'    ARMA errors, Trend and Seasonal components).
#'
#' @param .data Input data as \code{tsibble}.
#' @param specials Specials as list defined in \code{specials_tbats}.
#' @param periods Integer vector. The periodicity of the time series (e.g. \code{periods = c(24, 168)} for hourly data).
#' @param ... Further arguments passed to \code{forecast::tbats()}.
#'
#' @return An object of class \code{TBATS}.

train_tbats <- function(.data,
                        specials,
                        periods,
                        ...){

  if(length(tsibble::measured_vars(.data)) > 1){
    abort("Only univariate responses are supported by TBATS.")
  }

  # Prepare data for modelling
  y <- unclass(.data)[[measured_vars(.data)]]
  model_data <- msts(data = y, seasonal.periods = periods)

  if(any(is.na(model_data))){
    abort("TBATS does not support missing values.")
  }

  # Train model
  mdl <- forecast::tbats(y = model_data, ...)

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
    class = "TBATS")
}


specials_tbats <- new_specials()


#' @title Automatic training of a TBATS model
#'
#' @description Automatic training of a TBATS model (Trigonometric seasonality, Box-Cox transformation,
#'    ARMA errors, Trend and Seasonal components). This function is a wrapper for \code{forecast::tbats()}.
#'
#' @param formula Model specification (see "Specials" section, currently not in use...)
#' @param ... Further arguments passed to \code{forecast::tbats()}.
#'
#' @return tbats_model An object of class \code{TBATS}.
#' @export

TBATS <- function(formula, ...){
  tbats_model <- new_model_class(
    model = "TBATS",
    train = train_tbats,
    specials = specials_tbats)

  new_model_definition(
    tbats_model,
    !!enquo(formula),
    ...)
}


#' @title Forecast a trained TBATS model
#'
#' @description Forecast a trained TBATS model.
#'
#' @param object The trained TBATS model used to produce the forecasts.
#' @param new_data Forecast horizon.
#' @param specials Specials are currently not in use.
#' @param ... Additional arguments for forecast model method.
#'
#' @return An object of class "fable".
#' @export

forecast.TBATS <- function(object,
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


#' @title Extract fitted values from a TBATS
#'
#' @description Extract fitted values from a TBATS.
#'
#' @param object The time series model used to produce the forecast.
#' @param ... Further arguments.
#'
#' @return Fitted values as \code{tsibble}.
#' @export

fitted.TBATS <- function(object, ...){
  object$est[[".fitted"]]
}


#' @title Extract residuals from a TBATS
#'
#' @description Extract residuals from a TBATS.
#'
#' @param object The time series model used to produce the forecast.
#' @param ... Further arguments.
#'
#' @return Residuals as \code{tsibble}.
#' @export

residuals.TBATS <- function(object, ...){
  object$est[[".resid"]]
}


#' @title Provide a succinct summary of the TBATS
#'
#' @description Provide a succinct summary of the TBATS.
#'
#' @param x The TBATS to summarize.
#'
#' @return Model summary as character value.
#' @export

model_sum.TBATS <- function(x){
  "TBATS"
}
