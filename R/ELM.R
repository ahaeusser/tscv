
#' @title Train ELM neural network
#'
#' @description Train an Extreme Learning Machine (ELM).
#'
#' @param .data Input data as \code{tsibble}.
#' @param specials Specials as list defined in \code{specials_elm}.
#' @param ... Further arguments passed to \code{nnfor::elm()}.
#'
#' @return An object of class \code{ELM}.

train_elm <- function(.data,
                      specials,
                      ...){

  if(length(tsibble::measured_vars(.data)) > 1){
    abort("Only univariate responses are supported by ELM.")
  }

  # Prepare data for modeling
  model_data <- as.ts(.data)

  if(any(is.na(model_data))){
    abort("ELM does not support missing values.")
  }

  # Train model
  mdl <- nnfor::elm(y = model_data, ...)

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
    class = "ELM")
}


specials_elm <- new_specials()


#' @title Automatic training of ELMs
#'
#' @description Automatic training of Extreme Learning Machines (ELMs). This function
#'    is a wrapper for \code{nnfor::elm()}.
#'
#' @param formula Model specification (see "Specials" section, currently not in use...)
#' @param ... Further arguments passed to \code{nnfor::elm()}.
#'
#' @return elm_model An object of class \code{ELM}.
#' @export

ELM <- function(formula, ...){
  elm_model <- new_model_class(
    model = "ELM",
    train = train_elm,
    specials = specials_elm)

  new_model_definition(
    elm_model,
    !!enquo(formula),
    ...)
}


#' @title Forecast a trained ELM model
#'
#' @description Forecast a trained ELM model.
#'
#' @param object The trained ELM model used to produce the forecasts.
#' @param new_data Forecast horizon.
#' @param specials Specials are currently not in use.
#' @param ... Additional arguments for forecast model method.
#'
#' @return An object of class "fable".
#' @export

forecast.ELM <- function(object,
                         new_data,
                         specials = NULL,
                         ...){
  # Forecast model
  fcst <- forecast(object$model, h = nrow(new_data))

  # Extract point forecast and simulations
  mean <- fcst$mean
  sigma <- rowSds(fcst$all.mean)

  # Return forecasts
  construct_fc(
    point = mean,
    sd = sigma,
    dist = dist_normal(mean = mean, sd = sigma))
}


#' @title Extract fitted values from a ELM
#'
#' @description Extract fitted values from a ELM.
#'
#' @param object The time series model used to produce the forecast.
#' @param ... Further arguments.
#'
#' @return Fitted values as \code{tsibble}.
#' @export

fitted.ELM <- function(object, ...){
  object$est[[".fitted"]]
}


#' @title Extract residuals from a ELM
#'
#' @description Extract residuals from a ELM.
#'
#' @param object The time series model used to produce the forecast.
#' @param ... Further arguments.
#'
#' @return Residuals as \code{tsibble}.
#' @export

residuals.ELM <- function(object, ...){
  object$est[[".resid"]]
}


#' @title Provide a succinct summary of the ELM
#'
#' @description Provide a succinct summary of the ELM.
#'
#' @param x The ELM to summarize.
#'
#' @return Model summary as character value.
#' @export

model_sum.ELM <- function(x){
  "ELM"
}
