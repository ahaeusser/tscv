
#' @title Seasonal naive model
#'
#' @description Train a seasonal naive model (SNAIVE2).
#'
#' @param .data Input data as tsibble.
#' @param specials Specials as list defined in \code{specials_snaive2}.
#' @param ... Currently not in use.
#'
#' @return An object of class \code{SNAIVE2}.

train_snaive2 <- function(.data,
                          specials,
                          ...){

  if (length(measured_vars(.data)) > 1) {
    abort("Only univariate responses are supported by SNAIVE2.")
  }

  periods <- common_periods(.data)
  lag_day <- periods["day"]
  lag_week <- periods["week"]

  model_fit <- .data %>%
    mutate(day_of_week = wday(
      x = !!sym(index_var(.data)),
      week_start = getOption("lubridate.week.start", 1))) %>%
    mutate(fitted = ifelse(
      day_of_week %in% c(2, 3, 4, 5),
      dplyr::lag(!!sym(measured_vars(.data)), n = lag_day),
      dplyr::lag(!!sym(measured_vars(.data)), n = lag_week))) %>%
    mutate(resid = !!sym(measured_vars(.data)) - fitted)

  fitted <- model_fit[["fitted"]]
  resid <- model_fit[["resid"]]
  sigma <- sd(resid, na.rm = TRUE)

  model_spec <- list(
    data = .data,
    lag_day = lag_day,
    lag_week = lag_week
  )

  structure(
    list(
      model = model_spec,
      fitted = fitted,
      resid = resid,
      sigma = sigma),
    class = "SNAIVE2"
  )
}


#' @title Forecast a trained seasonal naive model
#'
#' @description Forecast a trained seasonal naive model.
#'
#' @param object An object of class \code{SNAIVE2}.
#' @param new_data Forecast horizon (n-step ahead forecast)
#' @param specials Specials are currently not in use.
#' @param ... Additional arguments for forecast method.
#'
#' @return An object of class \code{fable}.
#' @export

forecast.SNAIVE2 <- function(object,
                             new_data,
                             specials = NULL,
                             ...){

  # Forecast horizon
  n_ahead <- nrow(new_data)

  # Extract model
  data <- object$model$data
  lag_day <- object$model$lag_day
  lag_week <- object$model$lag_week

  # Seasonal naive forecast
  fcst <- data %>%
    append_row(n = n_ahead) %>%
    mutate(day_of_week = wday(
      x = !!sym(index_var(data)),
      week_start = getOption("lubridate.week.start", 1))) %>%
    mutate(point = ifelse(
      day_of_week %in% c(2, 3, 4, 5),
      dplyr::lag(!!sym(measured_vars(data)), n = lag_day),
      dplyr::lag(!!sym(measured_vars(data)), n = lag_week))) %>%
    slice_tail(n = n_ahead)

  point <- as.numeric(fcst[["point"]])
  sd <- as.numeric(rep(object$sigma, times = n_ahead))

  # Return forecasts
  dist_normal(point, sd)
}


specials_snaive2 <- new_specials()


#' @title Seasonal naive model
#'
#' @description Automatic train a seasonal naive model (SNAIVE2)..
#'
#' @param formula Model specification (see "Specials" section, currently not in use ...).
#' @param ... Further arguments.
#'
#' @return snaive2_model An object of class \code{SNAIVE2}.
#' @export

SNAIVE2 <- function(formula, ...){
  snaive2_model <- new_model_class(
    model = "SNAIVE2",
    train = train_snaive2,
    specials = specials_snaive2)

  new_model_definition(
    snaive2_model,
    !!enquo(formula),
    ...)
}


#' @title Extract fitted values from a trained seasonal naive model
#'
#' @description Extract fitted values from a trained seasonal naive model.
#'
#' @param object An object of class \code{SNAIVE2}.
#' @param ... Currently not in use.
#'
#' @return Fitted values as tsibble.
#' @export

fitted.SNAIVE2 <- function(object, ...){
  object[["fitted"]]
}


#' @title Extract residuals from a trained seasonal naive model
#'
#' @description Extract residuals from a trained seasonal naive model.
#'
#' @param object An object of class \code{SNAIVE2}.
#' @param ... Currently not in use.
#'
#' @return Fitted values as tsibble.
#' @export

residuals.SNAIVE2 <- function(object, ...){
  object[["resid"]]
}


#' @title Provide a succinct summary of a trained seasonal naive model
#'
#' @description Provide a succinct summary of a trained seasonal naive model.
#'
#' @param object An object of class \code{SNAIVE2}.
#'
#' @return Model summary as character value.
#' @export

model_sum.SNAIVE2 <- function(object){
  "SNAIVE2"
}
