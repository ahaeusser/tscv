
#' @title Extreme Learning Machine (ELM)
#'
#' @description Train an Extreme Learning Machine (ELM) model.
#'
#' @param .data Input data as tsibble.
#' @param specials Specials as list defined in \code{specials_elm}.
#' @param n_seed Integer value. The seed for the random number generator (for reproducibility).
#' @param ... Further arguments passed to \code{nnfor::elm()}.
#'
#' @return An object of class \code{ELM}.

train_elm <- function(.data,
                      specials,
                      n_seed = 42,
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
  set.seed(n_seed)
  model_fit <- nnfor::elm(y = model_data, ...)

  # Extract length of actual values and fitted values
  n_total <- length(model_fit$y)
  n_fitted <- length(model_fit$fitted)

  # Fill NAs in front of fitted values (adjust to equal length of actual values)
  fitted <- c(rep(NA_real_, n_total - n_fitted), model_fit$fitted)
  resid <- model_fit$y - fitted

  # Return model
  structure(
    list(
      model = model_fit,
      fitted = fitted,
      resid = resid),
    class = "ELM")
}


specials_elm <- new_specials()


#' @title Extreme Learning Machine (ELM)
#'
#' @description Automatic train an Extreme Learning Machines (ELMs) model.
#'   This function is a wrapper for \code{nnfor::elm()}.
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
#' @param object An object of class \code{ELM}.
#' @param new_data Forecast horizon (n-step ahead forecast)
#' @param specials Specials are currently not in use.
#' @param ... Additional arguments for forecast method.
#'
#' @return An object of class \code{fable}.
#' @export

forecast.ELM <- function(object,
                         new_data,
                         specials = NULL,
                         ...){
  # Forecast model
  fcst <- forecast::forecast(
    object$model,
    h = nrow(new_data)
    )

  # Extract point forecast
  point <- as.numeric(fcst$mean)
  sd <- rep(NA_real_, nrow(new_data))

  # Return forecasts
  dist_normal(point, sd)
}


#' @title Extract fitted values from a trained ELM model
#'
#' @description Extract fitted values from a trained ELM model.
#'
#' @param object An object of class \code{ELM}.
#' @param ... Currently not in use.
#'
#' @return Fitted values as tsibble.
#' @export

fitted.ELM <- function(object, ...){
  object[["fitted"]]
}


#' @title Extract residuals from a trained ELM model
#'
#' @description Extract residuals from a trained ELM model.
#'
#' @param object An object of class \code{ELM}.
#' @param ... Currently not in use.
#'
#' @return Fitted values as tsibble.
#' @export

residuals.ELM <- function(object, ...){
  object[["resid"]]
}


#' @title Provide a succinct summary of a trained ELM model
#'
#' @description Provide a succinct summary of a trained ELM model.
#'
#' @param object An object of class \code{ELM}.
#'
#' @return Model summary as character value.
#' @export

model_sum.ELM <- function(object){
  "ELM"
}
