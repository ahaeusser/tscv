
#' @title Median model
#'
#' @description Train median model (MEDIAN).
#'
#' @param .data Input data as tsibble.
#' @param specials Specials as list defined in \code{specials_median}.
#' @param ... Currently not in use.
#'
#' @return An object of class \code{MEDIAN}.

train_median <- function(.data,
                         specials,
                         ...){

  if (length(measured_vars(.data)) > 1) {
    abort("Only univariate responses are supported by MEDIAN.")
  }

  y <- unclass(.data)[[measured_vars(.data)]]

  if (all(is.na(y))) {
    abort("All observations are missing, a model cannot be estimated without data.")
  }

  n_obs <- length(y)
  window_size <- specials$window[[1]][["size"]]
  window_fixed <- specials$window[[1]][["fixed"]]

  if (is.null(window_size)) {
    median <- median(y, na.rm = TRUE)
    fitted <- rep(median, n_obs)
  }

  if (!is.null(window_size)) {
    if (window_fixed) {
      median <- median(
        tail(y, window_size),
        na.rm = TRUE
        )
      fitted <- rep(median, n_obs)
    } else {
      fitted <- slide_dbl(
        .x = y,
        .f = median,
        na.rm = TRUE,
        .size = window_size,
        .partial = TRUE
        )
      median <- fitted[length(fitted)]
      fitted <- dplyr::lag(fitted)
    }
  }

  resid <- y - fitted
  sigma <- sd(resid, na.rm = TRUE)

  structure(
    list(
      fitted = fitted,
      resid = resid,
      median = median,
      sigma = sigma,
      n_obs = n_obs,
      window = window_size %||% NA
    ),
    class = "MEDIAN"
  )
}


globalVariables(c("self", "origin"))

specials_median <- new_specials(
  window = function(size = NULL,
                    fixed = TRUE) {
    list(
      size = size,
      fixed = fixed
      )
  },
  .required_specials = "window"
)


#' @title Median model
#'
#' @description Automatic train a median model (MEDIAN).
#'
#' @param formula Model specification (see "Specials" section, currently not in use ...).
#' @param ... Further arguments.
#'
#' @return median_model An object of class \code{MEDIAN}.
#' @export

MEDIAN <- function(formula, ...){
  median_model <- new_model_class(
    model = "MEDIAN",
    train = train_median,
    specials = specials_median)

  new_model_definition(
    median_model,
    !!enquo(formula),
    ...)
}


#' @title Forecast a trained median model
#'
#' @description Forecast a trained median model.
#'
#' @param object An object of class \code{MEDIAN}.
#' @param new_data Forecast horizon (n-step ahead forecast)
#' @param specials Specials are currently not in use.
#' @param ... Additional arguments for forecast method.
#'
#' @return An object of class \code{fable}.
#' @export

forecast.MEDIAN <- function(object,
                            new_data,
                            specials = NULL,
                            ...){

  # Extract model components
  n_ahead <- nrow(new_data)
  n_obs <- object[["n_obs"]]
  median <- object[["median"]]
  sigma <- object[["sigma"]]

  point <- rep(median, n_ahead)
  sd <- sigma * sqrt(1 + 1 / n_obs)

  # Return forecasts
  dist_normal(point, sd)
}


#' @title Extract fitted values from a trained median model
#'
#' @description Extract fitted values from a trained median model.
#'
#' @param object An object of class \code{MEDIAN}.
#' @param ... Currently not in use.
#'
#' @return Fitted values as tsibble.
#' @export

fitted.MEDIAN <- function(object, ...){
  object[["fitted"]]
}


#' @title Extract residuals from a trained median model
#'
#' @description Extract residuals from a trained median model.
#'
#' @param object An object of class \code{MEDIAN}.
#' @param ... Currently not in use.
#'
#' @return Fitted values as tsibble.
#' @export

residuals.MEDIAN <- function(object, ...){
  object[["resid"]]
}


#' @title Provide a succinct summary of a trained median model
#'
#' @description Provide a succinct summary of a trained median model.
#'
#' @param object An object of class \code{MEDIAN}.
#'
#' @return Model summary as character value.
#' @export

model_sum.MEDIAN <- function(object){
  "MEDIAN"
}
