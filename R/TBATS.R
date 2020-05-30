
#' @title Train TBATS model.
#'
#' @description Train a TBATS model (Trigonometric, Box-Cox Transform, ...).
#'
#' @param .data Input data as \code{tsibble}.
#' @param specials Specials as list defined in \code{specials_tbats}.
#' @param ... Further arguments passed to \code{forecast::tbats()}.
#'
#' @return An object of class \code{TBATS}.

train_tbats <- function(.data,
                        specials,
                        ...){

  if(length(tsibble::measured_vars(.data)) > 1){
    abort("Only univariate responses are supported by TBATS.")
  }

  # Get seasonal periods (only two periods are possible)
  periods <- common_periods(.data)
  periods <- sort(as.numeric(periods))[1:2]

  # Prepare data for modelling
  y <- unclass(.data)[[measured_vars(.data)]]
  model_data <- msts(data = y, seasonal.periods = periods)

  if(any(is.na(model_data))){
    abort("TBATS does not support missing values.")
  }

  # Train model
  mdl <- forecast::tbats(y = model_data, ...)

  # Fill NAs in front of fitted values (adjust to equal length of actual values)
  fit <- mdl$fitted
  res <- mdl$residuals
  sigma <- sd(res, na.rm = TRUE)

  # Return model
  structure(
    list(
      model = mdl,
      est = list(
        .fitted = fit,
        .resid = res,
        sigma = sigma)),
    class = "TBATS")
}



specials_tbats <- new_specials()

#' @title Automatic training of TBATSs.
#'
#' @description Automatic training of TBATSs.
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


#' @title Forecast a trained TBATS model.
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
  # Extract model
  mdl <- object$model
  # Forecast model
  fcst <- forecast(mdl, h = nrow(new_data))

  # Extract point forecast and simulations
  mean <- as.numeric(fcst$mean)
  sigma <- as.numeric(object$est$sigma)

  # Return forecasts
  construct_fc(
    point = mean,
    sd = sigma,
    dist = dist_normal(mean = mean, sd =  rep(sigma, times = n_ahead))
    )
}


#' @title Extract fitted values from a TBATS.
#'
#' @description Extract fitted values from a TBATS.
#'
#' @param object The time series model used to produce the forecast.
#' @param ... Further arguments.
#'
#' @return
#' @export

fitted.TBATS <- function(object, ...){
  object$est[[".fitted"]]
}


#' @title Extract residuals from a TBATS.
#'
#' @description Extract residuals from a TBATS.
#'
#' @param object The time series model used to produce the forecast.
#' @param ... Further arguments.
#'
#' @return
#' @export

residuals.TBATS <- function(object, ...){
  object$est[[".resid"]]
}

#' @title Provide a succinct summary of the TBATS.
#'
#' @description Provide a succinct summary of the TBATS.
#'
#' @param object The TBATS to summarize.
#'
#' @return
#' @export

model_sum.TBATS <- function(x){
  "TBATS"
}
