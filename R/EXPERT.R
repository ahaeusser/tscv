
#' @title EXPERT model
#'
#' @description Train the EXPERT model.
#'
#' @param .data Input data as tsibble.
#' @param specials Specials as list defined in \code{specials_expert}.
#' @param periods Integer vector. The periodicity of the time series (e.g. \code{periods = c(24, 168)} for hourly data).
#' @param xreg A \code{tsibble} containing exogenous variables.
#' @param ... Further arguments (currently not in use).
#'
#' @return An object of class \code{EXPERT}.

train_expert <- function(.data,
                         specials = NULL,
                         periods,
                         xreg = NULL,
                         ...) {

  # Create features ...........................................................

  name_output <- measured_vars(.data)
  name_index <- index_var(.data)

  # Create lagged features
  data <- .data %>%
    append_row(n = min(periods)) %>%
    mutate(ymd = date(!!sym(name_index))) %>%
    mutate(hour = hour(!!sym(name_index))) %>%
    mutate("lag(1)" = dplyr::lag(!!sym(name_output), n = 1 * min(periods))) %>%  # (d-1)
    mutate("lag(2)" = dplyr::lag(!!sym(name_output), n = 2 * min(periods))) %>%  # (d-2)
    mutate("lag(7)" = dplyr::lag(!!sym(name_output), n = 7 * min(periods)))      # (d-7)

  # Drop leading missing values
  data <- data %>%
    slice(n = -c(1:max(periods)))

  # Create minimum and maximum of previous day
  data <- data %>%
    group_by(ymd) %>%
    mutate("min(1)" = min(.data$`lag(1)`, na.rm = TRUE)) %>%              # min(d-1)
    mutate("max(1)" = max(.data$`lag(1)`, na.rm = TRUE)) %>%              # max(d-1)
    ungroup()

  # Row indices for training and forecasting
  idx_total <- 1:nrow(data)
  idx_test <- tail(idx_total, n = min(periods))
  idx_train <- idx_total[-idx_test]


  # Prepare training and testing data .........................................
  data_train <- data %>%
    slice(idx_train)

  data_train <- data_train %>%
    group_by(hour) %>%
    group_split()

  data_test <- data %>%
    slice(idx_test)

  data_test <- data_test %>%
    group_by(hour) %>%
    group_split()

  # Train models
  models_fit <- map(
    .x = data_train,
    .f = ~lm(Value ~ `lag(1)` + `lag(2)` + `lag(7)` + `min(1)` + `max(1)`, .)
  )

  # Extract fitted values and residuals
  fitted <- NULL
  resid <- NULL
  sigma <- NULL

  # Return model
  structure(
    list(
      model = models_fit,
      test = data_test,
      est = list(
        .fitted = fitted,
        .resid = resid),
      sigma = sigma,
      periods = periods),
    class = "EXPERT")
}



specials_expert <- new_specials()


#' @title Automatic train a EXPERT model
#'
#' @description Automatic train the EXPERT model.
#'
#' @param formula Model specification (see "Specials" section, currently not in use ...)
#' @param ... Further arguments (currently not in use).
#'
#' @return expert_model An object of class \code{EXPERT}.
#' @export

EXPERT <- function(formula, ...){
  expert_model <- new_model_class(
    model = "EXPERT",
    train = train_expert,
    specials = specials_expert)

  new_model_definition(
    expert_model,
    !!enquo(formula),
    ...)
}


#' @title Forecast a trained EXPERT model
#'
#' @description Forecast a trained EXPERT model.
#'
#' @param object An object of class \code{EXPERT}.
#' @param new_data Forecast horizon (n-step ahead forecast)
#' @param specials Specials are currently not in use.
#' @param ... Additional arguments for forecast method.
#'
#' @return An object of class \code{fable}.
#' @export

forecast.EXPERT <- function(object,
                            new_data,
                            specials = NULL,
                            ...){

  # Forecast model
  fcst_point <- map_dbl(
    .x = 1:min(object[["periods"]]),
    .f = ~{
      predict(
        object = object[["model"]][[.x]],
        newdata = object[["test"]][[.x]]
      )
    }
  )

  fcst_std <- rep(NA, length(fcst_point))

  # Return forecasts
  dist_normal(fcst_point, fcst_std)
}





#' @title Extract fitted values from a trained EXPERT model
#'
#' @description Extract fitted values from a trained EXPERT model.
#'
#' @param object An object of class \code{EXPERT}.
#' @param ... Currently not in use.
#'
#' @return Fitted values as tsibble.
#' @export

fitted.EXPERT <- function(object, ...){
  object$est[[".fitted"]]
}


#' @title Extract residuals from a trained EXPERT model
#'
#' @description Extract residuals from a trained EXPERT model.
#'
#' @param object An object of class \code{EXPERT}.
#' @param ... Currently not in use.
#'
#' @return Fitted values as tsibble.
#' @export

residuals.EXPERT <- function(object, ...){
  object$est[[".resid"]]
}


#' @title Provide a succinct summary of a trained EXPERT model
#'
#' @description Provide a succinct summary of a trained EXPERT model.
#'
#' @param object An object of class \code{EXPERT}.
#'
#' @return Model summary as character value.
#' @export

model_sum.EXPERT <- function(object){
  "EXPERT"
}


