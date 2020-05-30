
#' @title Train DSHW model.
#'
#' @description Train a Double Seasonal Holt-Winters model (DSHW).
#'
#' @param .data Input data as \code{tsibble}.
#' @param specials Specials as list defined in \code{specials_dshw}.
#' @param ... Further arguments passed to \code{forecast::dshw()}.
#'
#' @return An object of class \code{DSHW}.

train_dshw <- function(.data,
                      specials,
                      ...){

  if(length(tsibble::measured_vars(.data)) > 1){
    abort("Only univariate responses are supported by DSHW.")
  }

  # Get seasonal periods (only two periods are possible)
  periods <- common_periods(.data)
  periods <- sort(as.numeric(periods))[1:2]

  # Prepare data for modelling
  y <- unclass(.data)[[measured_vars(.data)]]

  # Shift if y contains negative values
  # if(y_min < 0) {
  #   y <- y + abs(y_min) + 1
  # }

  model_data <- msts(data = y, seasonal.periods = periods)

  if(any(is.na(model_data))){
    abort("DSHW does not support missing values.")
  }

  # Train model
  mdl <- forecast::dshw(y = model_data, ...)

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
    class = "DSHW")
}



specials_dshw <- new_specials()

#' @title Automatic training of DSHWs.
#'
#' @description Automatic training of DSHWs.
#'
#' @param formula Model specification (see "Specials" section, currently not in use...)
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


#' @title Forecast a trained DSHW model.
#'
#' @description Forecast a trained DSHW model.
#'
#' @param object The trained DSHW model used to produce the forecasts.
#' @param new_data Forecast horizon.
#' @param specials Specials are currently not in use.
#' @param ... Additional arguments for forecast model method.
#'
#' @return An object of class "fable".
#' @export

forecast.DSHW <- function(object,
                         new_data,
                         specials = NULL,
                         ...){
  # Extract model
  mdl <- object$model
  # Forecast model
  fcst <- forecast(mdl, h = nrow(new_data))

  # Extract point forecast and simulations
  mean <- fcst$mean
  sigma <- object$sigma

  # Return forecasts
  construct_fc(
    point = mean,
    sd = sigma,
    dist = dist_normal(mean = mean, sd =  rep(sigma, times = n_ahead)))
}


#' @title Extract fitted values from a DSHW.
#'
#' @description Extract fitted values from a DSHW.
#'
#' @param object The time series model used to produce the forecast.
#' @param ... Further arguments.
#'
#' @return
#' @export

fitted.DSHW <- function(object, ...){
  object$est[[".fitted"]]
}


#' @title Extract residuals from a DSHW.
#'
#' @description Extract residuals from a DSHW.
#'
#' @param object The time series model used to produce the forecast.
#' @param ... Further arguments.
#'
#' @return
#' @export

residuals.DSHW <- function(object, ...){
  object$est[[".resid"]]
}

#' @title Provide a succinct summary of the DSHW.
#'
#' @description Provide a succinct summary of the DSHW.
#'
#' @param object The DSHW to summarize.
#'
#' @return
#' @export

model_sum.DSHW <- function(x){
  "DSHW"
}
