
#' @title Train seasonal median model.
#'
#' @description Train seasonal median model.
#'
#' @param data Input data as \code{tsibble}.
#' @param specials Specials as list defined in \code{specials_smedian}.
#' @param period Integer value. The seasonal period for the calculation.
#' @param ... Further arguments.
#'
#' @return An object of class \code{SMEDIAN}.

train_smedian <- function(.data,
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

  medians <- map_dbl(
    .x = 1:period,
    .f = ~median(y[index == .x], na.rm = TRUE)
  )

  fit <- rep(medians, times = ceiling(n / period))[1:n]
  res <- y - fit
  sigma <- sd(res, na.rm = TRUE)

  structure(
    list(
      .fitted = fit,
      .resid = res,
      median = medians,
      sigma = sigma,
      last_period = last(index),
      period = period
    ),
    class = "SMEDIAN"
  )
}



specials_smedian <- new_specials()


#' @title Automatic training of SMEDIANs.
#'
#' @description Automatic training of SMEDIANs.
#'
#' @param formula Model specification (see "Specials" section, currently not in use ...).
#' @param ... Further arguments.
#'
#' @return smedian_model An object of class \code{SMEDIAN}.
#' @export

SMEDIAN <- function(formula, ...){
  smedian_model <- new_model_class(
    model = "SMEDIAN",
    train = train_smedian,
    specials = specials_smedian)

  new_model_definition(
    smedian_model,
    !!enquo(formula),
    ...)
}


#' @title Forecast a trained SMEDIAN model.
#'
#' @description Forecast a trained SMEDIAN model.
#'
#' @param object Trained model.
#' @param new_data Forecast horizon.
#' @param specials Specials are currently not in use.
#' @param ... Further arguments.
#'
#' @return A \code{fable}.
#' @export

forecast.SMEDIAN <- function(object,
                             new_data,
                             specials = NULL,
                             ...){
  # Extract model
  medians <- object$median
  last_period <- object$last_period
  sigma <- object$sigma

  n_ahead <- nrow(new_data)
  index <- rep(1:length(medians), times = ceiling((n_ahead + last_period) / length(medians)))[(last_period + 1):(last_period + n_ahead)]
  fcst <- medians[index]

  # Return forecasts
  construct_fc(
    point = fcst,
    sd = rep(sigma, times = n_ahead),
    dist = dist_normal(mean = fcst, sd = rep(sigma, times = n_ahead)))
}


#' @title Extract fitted values from SMEDIAN.
#'
#' @description Extract fitted values from SMEDIAN.
#'
#' @param object The time series model used to produce the forecast.
#' @param ... Further arguments.
#'
#' @return
#' @export

fitted.SMEDIAN <- function(object, ...){
  object$est[[".fitted"]]
}


#' @title Extract residuals from SMEDIAN.
#'
#' @description Extract residuals from SMEDIAN.
#'
#' @param object The time series model used to produce the forecast.
#' @param ... Further arguments.
#'
#' @return
#' @export

residuals.SMEDIAN <- function(object, ...){
  object$est[[".resid"]]
}

#' @title Provide a succinct summary of the SMEDIAN.
#'
#' @description Provide a succinct summary of the SMEDIAN.
#'
#' @param object The SMEDIAN to summarize.
#'
#' @return
#' @export

model_sum.SMEDIAN <- function(x){
  "SMEDIAN"
}
