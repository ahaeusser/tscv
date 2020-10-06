
#' @title Train MLP neural network
#'
#' @description Train a Multilayer Perceptron (MLP).
#'
#' @param .data Input data as \code{tsibble}.
#' @param specials Specials as list defined in \code{specials_mlp}.
#' @param ... Further arguments passed to \code{nnfor::mlp()}.
#'
#' @return An object of class \code{MLP}.

train_mlp <- function(.data,
                      specials,
                      ...){

  if(length(tsibble::measured_vars(.data)) > 1){
    abort("Only univariate responses are supported by MLP.")
  }

  # Prepare data for modelling
  model_data <- as.ts(.data)

  if(any(is.na(model_data))){
    abort("MLP does not support missing values.")
  }

  # Train model
  mdl <- nnfor::mlp(y = model_data, ...)

  # Extract length of actual values and fitted values
  n_total <- length(mdl$y)
  n_fitted <- length(mdl$fitted)

  # Fill NAs in front of fitted values (adjust to equal length of actual values)
  fitted <- c(rep(NA_real_, n_total - n_fitted), mdl$fitted)
  resid <- mdl$y - fitted

  # Return model
  structure(
    list(
      model = mdl,
      est = list(
        .fitted = fitted,
        .resid = resid)),
    class = "MLP")
}


specials_mlp <- new_specials()

#' @title Automatic training of MLPs
#'
#' @description Automatic training of Multilayer Perceptrons (MLPs). This function
#'    is a wrapper for \code{nnfor::mlp()}.
#'
#' @param formula Model specification (see "Specials" section, currently not in use...)
#' @param ... Further arguments passed to \code{nnfor::mlp()}.
#'
#' @return mlp_model An object of class \code{MLP}.
#' @export

MLP <- function(formula, ...){
  mlp_model <- new_model_class(
    model = "MLP",
    train = train_mlp,
    specials = specials_mlp)

  new_model_definition(
    mlp_model,
    !!enquo(formula),
    ...)
}


#' @title Forecast a trained MLP model
#'
#' @description Forecast a trained MLP model.
#'
#' @param object The trained MLP model used to produce the forecasts.
#' @param new_data Forecast horizon.
#' @param specials Specials are currently not in use.
#' @param ... Additional arguments for forecast model method.
#'
#' @return An object of class "fable".
#' @export

forecast.MLP <- function(object,
                         new_data,
                         specials = NULL,
                         ...){
  # Forecast model
  fcst <- forecast(object$model, h = nrow(new_data))

  # Extract point forecast and simulations
  point <- as.numeric(fcst$mean)
  sd <- as.numeric(rowSds(fcst$all.mean))

  # Return forecasts
  dist_normal(point, sd)
}


#' @title Extract fitted values from a MLP.
#'
#' @description Extract fitted values from a MLP.
#'
#' @param object The time series model used to produce the forecast.
#' @param ... Further arguments.
#'
#' @return Fitted values as \code{tsibble}.
#' @export

fitted.MLP <- function(object, ...){
  object$est[[".fitted"]]
}


#' @title Extract residuals from a MLP
#'
#' @description Extract residuals from a MLP.
#'
#' @param object The time series model used to produce the forecast.
#' @param ... Further arguments.
#'
#' @return Residuals as \code{tsibble}.
#' @export

residuals.MLP <- function(object, ...){
  object$est[[".resid"]]
}


#' @title Provide a succinct summary of the MLP
#'
#' @description Provide a succinct summary of the MLP.
#'
#' @param x The MLP to summarize.
#'
#' @return Model summary as character value.
#' @export

model_sum.MLP <- function(x){
  "MLP"
}
