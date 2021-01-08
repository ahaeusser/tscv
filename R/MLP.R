
#' @title Multilayer Perceptron (MLP)
#'
#' @description Train a Multilayer Perceptron (MLP) model.
#'
#' @param .data Input data as tsibble.
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

  # Prepare data for modeling
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
#' @description Automatic train a Multilayer Perceptron (MLPs) model.
#'   This function is a wrapper for \code{nnfor::mlp()}.
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
#' @param object An object of class \code{MLP}.
#' @param new_data Forecast horizon (n-step ahead forecast)
#' @param specials Specials are currently not in use.
#' @param ... Additional arguments for forecast method.
#'
#' @return An object of class \code{fable}.
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


#' @title Extract fitted values from a trained MLP model
#'
#' @description Extract fitted values from a trained MLP model.
#'
#' @param object An object of class \code{MLP}.
#' @param ... Currently not in use.
#'
#' @return Fitted values as tsibble.
#' @export

fitted.MLP <- function(object, ...){
  object$est[[".fitted"]]
}


#' @title Extract residuals from a trained MLP model
#'
#' @description Extract residuals from a trained MLP model.
#'
#' @param object An object of class \code{MLP}.
#' @param ... Currently not in use.
#'
#' @return Fitted values as tsibble.
#' @export

residuals.MLP <- function(object, ...){
  object$est[[".resid"]]
}


#' @title Provide a succinct summary of a trained MLP model
#'
#' @description Provide a succinct summary of a trained MLP model.
#'
#' @param object An object of class \code{MLP}.
#'
#' @return Model summary as character value.
#' @export

model_sum.MLP <- function(object){
  "MLP"
}
