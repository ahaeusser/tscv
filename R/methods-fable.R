
#' @title Train a TBATS model
#'
#' @description
#' Internal training function for \code{TBATS()}.
#'
#' @param specials Parsed model specials. Currently not used.
#' @param periods Integer vector giving the seasonal periods of the time series,
#'   for example \code{c(24, 168)} for hourly data with daily and weekly
#'   seasonality.
#' @param ... Further arguments passed to \code{forecast::tbats()}.
#'
#' @return An object of class \code{"TBATS"}.
#' @noRd

train_tbats <- function(.data,
                        specials,
                        periods,
                        ...){

  if(length(tsibble::measured_vars(.data)) > 1){
    abort("Only univariate responses are supported by TBATS.")
  }

  # Prepare data for modeling
  y <- unclass(.data)[[measured_vars(.data)]]
  model_data <- msts(data = y, seasonal.periods = periods)

  if(any(is.na(model_data))){
    abort("TBATS does not support missing values.")
  }

  # Train model
  model_fit <- forecast::tbats(y = model_data, ...)

  # Extract fitted values and residuals
  fitted <- model_fit$fitted
  resid <- model_fit$residuals
  sigma <- sd(resid, na.rm = TRUE)

  # Return model
  structure(
    list(
      model = model_fit,
      fitted = fitted,
      resid = resid,
      sigma = sigma),
    class = "TBATS")
}


specials_tbats <- new_specials()


#' @title TBATS model
#'
#' @description
#' Specify a TBATS model for use with \code{fabletools::model()}.
#'
#' @details
#' \code{TBATS()} is a model specification wrapper around
#' \code{forecast::tbats()} for the \code{fable}, \code{tsibble}, and
#' \code{fabletools} ecosystem.
#'
#' TBATS stands for trigonometric seasonality, Box-Cox transformation, ARMA
#' errors, trend, and seasonal components. It can be useful for time series with
#' multiple or complex seasonal patterns.
#'
#' The seasonal periods must be supplied through the \code{periods} argument.
#'
#' @param formula A model formula specifying the response variable, for example
#'   \code{value}.
#' @param ... Further arguments passed to \code{forecast::tbats()}, including
#'   \code{periods}.
#'
#' @return
#' A model definition that can be used inside \code{fabletools::model()}.
#'
#' @family TBATS
#' @export
#'
#' @examples
#' \donttest{
#' library(dplyr)
#' library(tsibble)
#' library(fabletools)
#'
#' train_frame <- elec_price |>
#'   filter(bidding_zone == "DE") |>
#'   slice_head(n = 24 * 21) |>
#'   as_tsibble(index = time)
#'
#' model_frame <- train_frame |>
#'   model("TBATS" = TBATS(value, periods = c(24, 168)))
#'
#' model_frame
#' }

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


#' @title Forecast a TBATS model
#'
#' @description
#' Forecast a fitted \code{TBATS} model.
#'
#' @details
#' This method is used by \code{forecast()} when forecasting a mable containing
#' a \code{TBATS} model.
#'
#' @param object A fitted \code{TBATS} model object.
#' @param new_data A \code{tsibble} containing future time points.
#' @param specials Parsed specials. Currently not used.
#' @param ... Additional arguments. Currently not used.
#'
#' @return
#' A vector of forecast distributions.
#'
#' @family TBATS
#' @export
#'
#' @examples
#' \donttest{
#' library(dplyr)
#' library(tsibble)
#' library(fabletools)
#'
#' train_frame <- elec_price |>
#'   filter(bidding_zone == "DE") |>
#'   slice_head(n = 24 * 21) |>
#'   as_tsibble(index = time)
#'
#' model_frame <- train_frame |>
#'   model("TBATS" = TBATS(value, periods = c(24, 168)))
#'
#' forecast(model_frame, h = 24)
#' }

forecast.TBATS <- function(object,
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


#' @title Extract fitted values from a TBATS model
#'
#' @description
#' Extract fitted values from a fitted \code{TBATS} model.
#'
#' @details
#' This method is used by \code{fitted()} when extracting fitted values from a
#' mable containing a \code{TBATS} model.
#'
#' @param object A fitted \code{TBATS} model object.
#' @param ... Additional arguments. Currently not used.
#'
#' @return
#' Fitted values.
#'
#' @family TBATS
#' @export
#'
#' @examples
#' \donttest{
#' library(dplyr)
#' library(tsibble)
#' library(fabletools)
#'
#' train_frame <- elec_price |>
#'   filter(bidding_zone == "DE") |>
#'   slice_head(n = 24 * 21) |>
#'   as_tsibble(index = time)
#'
#' model_frame <- train_frame |>
#'   model("TBATS" = TBATS(value, periods = c(24, 168)))
#'
#' fitted(model_frame)
#' }

fitted.TBATS <- function(object, ...){
  object[["fitted"]]
}


#' @title Extract residuals from a TBATS model
#'
#' @description
#' Extract residuals from a fitted \code{TBATS} model.
#'
#' @details
#' This method is used by \code{residuals()} when extracting residuals from a
#' mable containing a \code{TBATS} model.
#'
#' @param object A fitted \code{TBATS} model object.
#' @param ... Additional arguments. Currently not used.
#'
#' @return
#' Residuals.
#'
#' @family TBATS
#' @export
#'
#' @examples
#' \donttest{
#' library(dplyr)
#' library(tsibble)
#' library(fabletools)
#'
#' train_frame <- elec_price |>
#'   filter(bidding_zone == "DE") |>
#'   slice_head(n = 24 * 21) |>
#'   as_tsibble(index = time)
#'
#' model_frame <- train_frame |>
#'   model("TBATS" = TBATS(value, periods = c(24, 168)))
#'
#' residuals(model_frame)
#' }

residuals.TBATS <- function(object, ...){
  object[["resid"]]
}


#' @title Summarize a TBATS model
#'
#' @description
#' Return a short model label for a fitted \code{TBATS} model.
#'
#' @param x A fitted \code{TBATS} model object.
#'
#' @return
#' A character string.
#'
#' @family TBATS
#' @export
#'
#' @examples
#' \donttest{
#' library(dplyr)
#' library(tsibble)
#' library(fabletools)
#'
#' train_frame <- elec_price |>
#'   filter(bidding_zone == "DE") |>
#'   slice_head(n = 24 * 21) |>
#'   as_tsibble(index = time)
#'
#' model_frame <- train_frame |>
#'   model("TBATS" = TBATS(value, periods = c(24, 168)))
#'
#' model_frame
#' }

model_sum.TBATS <- function(x){
  "TBATS"
}



#' @title Train a Double Seasonal Holt-Winters model
#'
#' @description
#' Internal training function for \code{DSHW()}.
#'
#' @param .data A \code{tsibble} containing the response variable.
#' @param specials Parsed model specials. Currently not used.
#' @param periods Integer vector giving the seasonal periods of the time series,
#'   for example \code{c(24, 168)} for hourly data with daily and weekly
#'   seasonality.
#' @param ... Further arguments passed to \code{forecast::dshw()}.
#'
#' @return An object of class \code{"DSHW"}.
#' @noRd

train_dshw <- function(.data,
                       specials,
                       periods,
                       ...){

  if(length(tsibble::measured_vars(.data)) > 1){
    abort("Only univariate responses are supported by DSHW.")
  }

  # Prepare data for modeling
  y <- unclass(.data)[[measured_vars(.data)]]
  model_data <- msts(data = y, seasonal.periods = periods)

  if(any(is.na(model_data))){
    abort("DSHW does not support missing values.")
  }

  # Train model
  model_fit <- forecast::dshw(y = model_data, ...)

  # Extract fitted values and residuals
  fitted <- model_fit$fitted
  resid <- model_fit$residuals
  sigma <- sd(resid, na.rm = TRUE)

  # Return model
  structure(
    list(
      model = model_fit,
      fitted = fitted,
      resid = resid,
      sigma = sigma),
    class = "DSHW")
}


specials_dshw <- new_specials()


#' @title Double Seasonal Holt-Winters model
#'
#' @description
#' Specify a Double Seasonal Holt-Winters model for use with
#' \code{fabletools::model()}.
#'
#' @details
#' \code{DSHW()} is a model specification wrapper around
#' \code{forecast::dshw()} for the \code{fable}, \code{tsibble}, and
#' \code{fabletools} ecosystem.
#'
#' The model is useful for time series with two important seasonal patterns,
#' such as hourly data with daily and weekly seasonality.
#'
#' The seasonal periods must be supplied through the \code{periods} argument.
#'
#' @param formula A model formula specifying the response variable, for example
#'   \code{value}.
#' @param ... Further arguments passed to \code{forecast::dshw()}, including
#'   \code{periods}.
#'
#' @return
#' A model definition that can be used inside \code{fabletools::model()}.
#'
#' @family DSHW
#' @export
#'
#' @examples
#' \donttest{
#' library(dplyr)
#' library(tsibble)
#' library(fabletools)
#'
#' train_frame <- elec_load |>
#'   filter(bidding_zone == "DE") |>
#'   slice_head(n = 24 * 28) |>
#'   as_tsibble(index = time)
#'
#' model_frame <- train_frame |>
#'   model("DSHW" = DSHW(value, periods = c(24, 168)))
#'
#' model_frame
#' }

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


#' @title Forecast a DSHW model
#'
#' @description
#' Forecast a fitted \code{DSHW} model.
#'
#' @param object A fitted \code{DSHW} model object.
#' @param new_data A \code{tsibble} containing future time points.
#' @param specials Parsed specials. Currently not used.
#' @param ... Additional arguments. Currently not used.
#'
#' @return
#' A vector of forecast distributions.
#'
#' @family DSHW
#' @export
#'
#' @examples
#' \donttest{
#' library(dplyr)
#' library(tsibble)
#' library(fabletools)
#'
#' train_frame <- elec_load |>
#'   filter(bidding_zone == "DE") |>
#'   slice_head(n = 24 * 28) |>
#'   as_tsibble(index = time)
#'
#' model_frame <- train_frame |>
#'   model("DSHW" = DSHW(value, periods = c(24, 168)))
#'
#' forecast(model_frame, h = 24)
#' }

forecast.DSHW <- function(object,
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


#' @title Extract fitted values from a DSHW model
#'
#' @description
#' Extract fitted values from a fitted \code{DSHW} model.
#'
#' @param object A fitted \code{DSHW} model object.
#' @param ... Additional arguments. Currently not used.
#'
#' @return
#' Fitted values.
#'
#' @family DSHW
#' @export
#'
#' @examples
#' \donttest{
#' library(dplyr)
#' library(tsibble)
#' library(fabletools)
#'
#' train_frame <- elec_load |>
#'   filter(bidding_zone == "DE") |>
#'   slice_head(n = 24 * 28) |>
#'   as_tsibble(index = time)
#'
#' model_frame <- train_frame |>
#'   model("DSHW" = DSHW(value, periods = c(24, 168)))
#'
#' fitted(model_frame)
#' }

fitted.DSHW <- function(object, ...){
  object[["fitted"]]
}


#' @title Extract residuals from a DSHW model
#'
#' @description
#' Extract residuals from a fitted \code{DSHW} model.
#'
#' @param object A fitted \code{DSHW} model object.
#' @param ... Additional arguments. Currently not used.
#'
#' @return
#' Residuals.
#'
#' @family DSHW
#' @export
#'
#' @examples
#' \donttest{
#' library(dplyr)
#' library(tsibble)
#' library(fabletools)
#'
#' train_frame <- elec_load |>
#'   filter(bidding_zone == "DE") |>
#'   slice_head(n = 24 * 28) |>
#'   as_tsibble(index = time)
#'
#' model_frame <- train_frame |>
#'   model("DSHW" = DSHW(value, periods = c(24, 168)))
#'
#' residuals(model_frame)
#' }

residuals.DSHW <- function(object, ...){
  object[["resid"]]
}


#' @title Summarize a DSHW model
#'
#' @description
#' Return a short model label for a fitted \code{DSHW} model.
#'
#' @param x A fitted \code{DSHW} model object.
#'
#' @return
#' A character string.
#'
#' @family DSHW
#' @export
#'
#' @examples
#' \donttest{
#' library(dplyr)
#' library(tsibble)
#' library(fabletools)
#'
#' train_frame <- elec_load |>
#'   filter(bidding_zone == "DE") |>
#'   slice_head(n = 24 * 28) |>
#'   as_tsibble(index = time)
#'
#' model_frame <- train_frame |>
#'   model("DSHW" = DSHW(value, periods = c(24, 168)))
#'
#' model_frame
#' }

model_sum.DSHW <- function(x){
  "DSHW"
}



#' @title Train a median model
#'
#' @description
#' Internal training function for \code{MEDIAN()}.
#'
#' @param .data A \code{tsibble} containing the response variable.
#' @param specials Parsed model specials created by \code{specials_median}.
#' @param ... Currently not used.
#'
#' @return An object of class \code{"MEDIAN"}.
#' @noRd

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
#' @description
#' Specify a median benchmark model for use with \code{fabletools::model()}.
#'
#' @details
#' \code{MEDIAN()} forecasts future values using the median of the observed
#' response. The \code{window()} special controls whether the median is estimated
#' using all observations, a fixed trailing window, or a rolling window.
#'
#' @param formula A model formula specifying the response and optional
#'   \code{window()} special, for example \code{value ~ window()}.
#' @param ... Further arguments.
#'
#' @return
#' A model definition that can be used inside \code{fabletools::model()}.
#'
#' @family MEDIAN
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tsibble)
#' library(fabletools)
#'
#' train_frame <- M4_monthly_data |>
#'   filter(series == first(series)) |>
#'   as_tsibble(index = index)
#'
#' model_frame <- train_frame |>
#'   model("MEDIAN" = MEDIAN(value ~ window()))
#'
#' model_frame

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


#' @title Forecast a median model
#'
#' @description
#' Forecast a fitted \code{MEDIAN} model.
#'
#' @param object A fitted \code{MEDIAN} model object.
#' @param new_data A \code{tsibble} containing future time points.
#' @param specials Parsed specials. Currently not used.
#' @param ... Additional arguments. Currently not used.
#'
#' @return
#' A vector of forecast distributions.
#'
#' @family MEDIAN
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tsibble)
#' library(fabletools)
#'
#' train_frame <- M4_monthly_data |>
#'   filter(series == first(series)) |>
#'   as_tsibble(index = index)
#'
#' model_frame <- train_frame |>
#'   model("MEDIAN" = MEDIAN(value ~ window()))
#'
#' forecast(model_frame, h = 12)

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


#' @title Extract fitted values from a median model
#'
#' @description
#' Extract fitted values from a fitted \code{MEDIAN} model.
#'
#' @param object A fitted \code{MEDIAN} model object.
#' @param ... Additional arguments. Currently not used.
#'
#' @return
#' Fitted values.
#'
#' @family MEDIAN
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tsibble)
#' library(fabletools)
#'
#' train_frame <- M4_monthly_data |>
#'   filter(series == first(series)) |>
#'   as_tsibble(index = index)
#'
#' model_frame <- train_frame |>
#'   model("MEDIAN" = MEDIAN(value ~ window()))
#'
#' fitted(model_frame)

fitted.MEDIAN <- function(object, ...){
  object[["fitted"]]
}


#' @title Extract residuals from a median model
#'
#' @description
#' Extract residuals from a fitted \code{MEDIAN} model.
#'
#' @param object A fitted \code{MEDIAN} model object.
#' @param ... Additional arguments. Currently not used.
#'
#' @return
#' Residuals.
#'
#' @family MEDIAN
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tsibble)
#' library(fabletools)
#'
#' train_frame <- M4_monthly_data |>
#'   filter(series == first(series)) |>
#'   as_tsibble(index = index)
#'
#' model_frame <- train_frame |>
#'   model("MEDIAN" = MEDIAN(value ~ window()))
#'
#' residuals(model_frame)

residuals.MEDIAN <- function(object, ...){
  object[["resid"]]
}


#' @title Summarize a median model
#'
#' @description
#' Return a short model label for a fitted \code{MEDIAN} model.
#'
#' @param x A fitted \code{MEDIAN} model object.
#'
#' @return
#' A character string.
#'
#' @family MEDIAN
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tsibble)
#' library(fabletools)
#'
#' train_frame <- M4_monthly_data |>
#'   filter(series == first(series)) |>
#'   as_tsibble(index = index)
#'
#' model_frame <- train_frame |>
#'   model("MEDIAN" = MEDIAN(value ~ window()))
#'
#' model_frame

model_sum.MEDIAN <- function(x){
  "MEDIAN"
}



#' @title Train a seasonal mean model
#'
#' @description
#' Internal training function for \code{SMEAN()}.
#'
#' @param .data A \code{tsibble} containing the response variable.
#' @param specials Parsed model specials created by \code{specials_smean}.
#' @param ... Currently not used.
#'
#' @return An object of class \code{"SMEAN"}.
#' @noRd

train_smean <- function(.data,
                        specials,
                        ...){

  if (length(measured_vars(.data)) > 1) {
    abort("Only univariate responses are supported by SMEAN.")
  }

  y <- unclass(.data)[[measured_vars(.data)]]

  if (all(is.na(y))) {
    abort("All observations are missing, a model cannot be estimated without data.")
  }

  # Prepare period
  lag <- specials$lag[[1]]
  n <- length(y)
  index <- rep(1:lag, times = ceiling(n / lag))[1:n]

  smean <- map_dbl(
    .x = 1:lag,
    .f = ~mean(y[index == .x], na.rm = TRUE)
  )

  fitted <- rep(smean, times = ceiling(n / lag))[1:n]
  resid <- y - fitted
  sigma <- sd(resid, na.rm = TRUE)

  structure(
    list(
      fitted = fitted,
      resid = resid,
      smean = smean,
      sigma = sigma,
      last_period = last(index),
      lag = lag),
    class = "SMEAN"
  )
}


specials_smean <- new_specials(
  lag = function(lag = NULL) {
    lag <- get_frequencies(period = lag, data = self$data, .auto = "smallest")
    if (lag == 1) {
      abort("Non-seasonal model specification provided, use RW() or provide a different lag specification.")
    }
    if (!rlang::is_integerish(lag)) {
      warn("Non-integer lag orders for random walk models are not supported. Rounding to the nearest integer.")
      lag <- round(lag)
    }
    lag
  },
  .required_specials = c("lag")
)


#' @title Seasonal mean model
#'
#' @description
#' Specify a seasonal mean benchmark model for use with
#' \code{fabletools::model()}.
#'
#' @details
#' \code{SMEAN()} forecasts each future observation using the historical mean of
#' the matching seasonal position. Use the \code{lag()} special to define the
#' seasonal period, for example \code{lag("year")} for monthly data.
#'
#' @param formula A model formula specifying the response and \code{lag()}
#'   special, for example \code{value ~ lag("year")}.
#' @param ... Further arguments.
#'
#' @return
#' A model definition that can be used inside \code{fabletools::model()}.
#'
#' @family SMEAN
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tsibble)
#' library(fabletools)
#'
#' train_frame <- M4_monthly_data |>
#'   filter(series == first(series)) |>
#'   as_tsibble(index = index)
#'
#' model_frame <- train_frame |>
#'   model("SMEAN" = SMEAN(value ~ lag("year")))
#'
#' model_frame

SMEAN <- function(formula, ...){
  smean_model <- new_model_class(
    model = "SMEAN",
    train = train_smean,
    specials = specials_smean)

  new_model_definition(
    smean_model,
    !!enquo(formula),
    ...)
}


#' @title Forecast a seasonal mean model
#'
#' @description
#' Forecast a fitted \code{SMEAN} model.
#'
#' @param object A fitted \code{SMEAN} model object.
#' @param new_data A \code{tsibble} containing future time points.
#' @param specials Parsed specials. Currently not used.
#' @param ... Additional arguments. Currently not used.
#'
#' @return
#' A vector of forecast distributions.
#'
#' @family SMEAN
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tsibble)
#' library(fabletools)
#'
#' train_frame <- M4_monthly_data |>
#'   filter(series == first(series)) |>
#'   as_tsibble(index = index)
#'
#' model_frame <- train_frame |>
#'   model("SMEAN" = SMEAN(value ~ lag("year")))
#'
#' forecast(model_frame, h = 12)

forecast.SMEAN <- function(object,
                           new_data,
                           specials = NULL,
                           ...){
  # Extract model
  smean <- object$smean
  last_period <- object$last_period

  n_ahead <- nrow(new_data)
  index <- rep(
    1:length(smean),
    times = ceiling((n_ahead + last_period) / length(smean))
  )[(last_period + 1):(last_period + n_ahead)]
  point <- as.numeric(smean[index])
  sd <- as.numeric(rep(object$sigma, times = n_ahead))

  # Return forecasts
  dist_normal(point, sd)
}


#' @title Extract fitted values from a seasonal mean model
#'
#' @description
#' Extract fitted values from a fitted \code{SMEAN} model.
#'
#' @param object A fitted \code{SMEAN} model object.
#' @param ... Additional arguments. Currently not used.
#'
#' @return
#' Fitted values.
#'
#' @family SMEAN
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tsibble)
#' library(fabletools)
#'
#' train_frame <- M4_monthly_data |>
#'   filter(series == first(series)) |>
#'   as_tsibble(index = index)
#'
#' model_frame <- train_frame |>
#'   model("SMEAN" = SMEAN(value ~ lag("year")))
#'
#' fitted(model_frame)

fitted.SMEAN <- function(object, ...){
  object[["fitted"]]
}


#' @title Extract residuals from a seasonal mean model
#'
#' @description
#' Extract residuals from a fitted \code{SMEAN} model.
#'
#' @param object A fitted \code{SMEAN} model object.
#' @param ... Additional arguments. Currently not used.
#'
#' @return
#' Residuals.
#'
#' @family SMEAN
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tsibble)
#' library(fabletools)
#'
#' train_frame <- M4_monthly_data |>
#'   filter(series == first(series)) |>
#'   as_tsibble(index = index)
#'
#' model_frame <- train_frame |>
#'   model("SMEAN" = SMEAN(value ~ lag("year")))
#'
#' residuals(model_frame)

residuals.SMEAN <- function(object, ...){
  object[["resid"]]
}


#' @title Summarize a seasonal mean model
#'
#' @description
#' Return a short model label for a fitted \code{SMEAN} model.
#'
#' @param x A fitted \code{SMEAN} model object.
#'
#' @return
#' A character string.
#'
#' @family SMEAN
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tsibble)
#' library(fabletools)
#'
#' train_frame <- M4_monthly_data |>
#'   filter(series == first(series)) |>
#'   as_tsibble(index = index)
#'
#' model_frame <- train_frame |>
#'   model("SMEAN" = SMEAN(value ~ lag("year")))
#'
#' model_frame

model_sum.SMEAN <- function(x){
  "SMEAN"
}



#' @title Train a seasonal median model
#'
#' @description
#' Internal training function for \code{SMEDIAN()}.
#'
#' @param .data A \code{tsibble} containing the response variable.
#' @param specials Parsed model specials created by \code{specials_smedian}.
#' @param ... Currently not used.
#'
#' @return An object of class \code{"SMEDIAN"}.
#' @noRd

train_smedian <- function(.data,
                          specials,
                          ...){

  if (length(measured_vars(.data)) > 1) {
    abort("Only univariate responses are supported by SMEDIAN.")
  }

  y <- unclass(.data)[[measured_vars(.data)]]

  if (all(is.na(y))) {
    abort("All observations are missing, a model cannot be estimated without data.")
  }

  # Prepare period
  lag <- specials$lag[[1]]
  n <- length(y)
  index <- rep(1:lag, times = ceiling(n / lag))[1:n]

  smedian <- map_dbl(
    .x = 1:lag,
    .f = ~median(y[index == .x], na.rm = TRUE)
  )

  fitted <- rep(smedian, times = ceiling(n / lag))[1:n]
  resid <- y - fitted
  sigma <- sd(resid, na.rm = TRUE)

  structure(
    list(
      fitted = fitted,
      resid = resid,
      smedian = smedian,
      sigma = sigma,
      last_period = last(index),
      lag = lag),
    class = "SMEDIAN"
  )
}


globalVariables(c("self", "origin"))

specials_smedian <- new_specials(
  lag = function(lag = NULL) {
    lag <- get_frequencies(period = lag, data = self$data, .auto = "smallest")
    if (lag == 1) {
      abort("Non-seasonal model specification provided, use RW() or provide a different lag specification.")
    }
    if (!rlang::is_integerish(lag)) {
      warn("Non-integer lag orders for random walk models are not supported. Rounding to the nearest integer.")
      lag <- round(lag)
    }
    lag
  },
  .required_specials = c("lag")
)


#' @title Seasonal median model
#'
#' @description
#' Specify a seasonal median benchmark model for use with
#' \code{fabletools::model()}.
#'
#' @details
#' \code{SMEDIAN()} forecasts each future observation using the historical median
#' of the matching seasonal position. Use the \code{lag()} special to define the
#' seasonal period, for example \code{lag("week")} for hourly data with weekly
#' seasonality.
#'
#' @param formula A model formula specifying the response and \code{lag()}
#'   special, for example \code{value ~ lag("week")}.
#' @param ... Further arguments.
#'
#' @return
#' A model definition that can be used inside \code{fabletools::model()}.
#'
#' @family SMEDIAN
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tsibble)
#' library(fabletools)
#'
#' train_frame <- elec_price |>
#'   filter(bidding_zone == "DE") |>
#'   slice_head(n = 24 * 21) |>
#'   as_tsibble(index = time)
#'
#' model_frame <- train_frame |>
#'   model("SMEDIAN" = SMEDIAN(value ~ lag("week")))
#'
#' model_frame

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


#' @title Forecast a seasonal median model
#'
#' @description
#' Forecast a fitted \code{SMEDIAN} model.
#'
#' @param object A fitted \code{SMEDIAN} model object.
#' @param new_data A \code{tsibble} containing future time points.
#' @param specials Parsed specials. Currently not used.
#' @param ... Additional arguments. Currently not used.
#'
#' @return
#' A vector of forecast distributions.
#'
#' @family SMEDIAN
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tsibble)
#' library(fabletools)
#'
#' train_frame <- elec_price |>
#'   filter(bidding_zone == "DE") |>
#'   slice_head(n = 24 * 21) |>
#'   as_tsibble(index = time)
#'
#' model_frame <- train_frame |>
#'   model("SMEDIAN" = SMEDIAN(value ~ lag("week")))
#'
#' forecast(model_frame, h = 24)

forecast.SMEDIAN <- function(object,
                             new_data,
                             specials = NULL,
                             ...){
  # Extract model
  smedian <- object$smedian
  last_period <- object$last_period

  n_ahead <- nrow(new_data)
  index <- rep(
    1:length(smedian),
    times = ceiling((n_ahead + last_period) / length(smedian))
  )[(last_period + 1):(last_period + n_ahead)]
  point <- as.numeric(smedian[index])
  sd <- as.numeric(rep(object$sigma, times = n_ahead))

  # Return forecasts
  dist_normal(point, sd)

}


#' @title Extract fitted values from a seasonal median model
#'
#' @description
#' Extract fitted values from a fitted \code{SMEDIAN} model.
#'
#' @param object A fitted \code{SMEDIAN} model object.
#' @param ... Additional arguments. Currently not used.
#'
#' @return
#' Fitted values.
#'
#' @family SMEDIAN
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tsibble)
#' library(fabletools)
#'
#' train_frame <- elec_price |>
#'   filter(bidding_zone == "DE") |>
#'   slice_head(n = 24 * 21) |>
#'   as_tsibble(index = time)
#'
#' model_frame <- train_frame |>
#'   model("SMEDIAN" = SMEDIAN(value ~ lag("week")))
#'
#' fitted(model_frame)

fitted.SMEDIAN <- function(object, ...){
  object[["fitted"]]
}


#' @title Extract residuals from a seasonal median model
#'
#' @description
#' Extract residuals from a fitted \code{SMEDIAN} model.
#'
#' @param object A fitted \code{SMEDIAN} model object.
#' @param ... Additional arguments. Currently not used.
#'
#' @return
#' Residuals.
#'
#' @family SMEDIAN
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tsibble)
#' library(fabletools)
#'
#' train_frame <- elec_price |>
#'   filter(bidding_zone == "DE") |>
#'   slice_head(n = 24 * 21) |>
#'   as_tsibble(index = time)
#'
#' model_frame <- train_frame |>
#'   model("SMEDIAN" = SMEDIAN(value ~ lag("week")))
#'
#' residuals(model_frame)

residuals.SMEDIAN <- function(object, ...){
  object[["resid"]]
}


#' @title Summarize a seasonal median model
#'
#' @description
#' Return a short model label for a fitted \code{SMEDIAN} model.
#'
#' @param x A fitted \code{SMEDIAN} model object.
#'
#' @return
#' A character string.
#'
#' @family SMEDIAN
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tsibble)
#' library(fabletools)
#'
#' train_frame <- elec_price |>
#'   filter(bidding_zone == "DE") |>
#'   slice_head(n = 24 * 21) |>
#'   as_tsibble(index = time)
#'
#' model_frame <- train_frame |>
#'   model("SMEDIAN" = SMEDIAN(value ~ lag("week")))
#'
#' model_frame

model_sum.SMEDIAN <- function(x){
  "SMEDIAN"
}



#' @title Train a seasonal naive model with weekday-specific lags
#'
#' @description
#' Internal training function for \code{SNAIVE2()}.
#'
#' @param .data A \code{tsibble} containing one response variable and an hourly
#'   index.
#' @param specials Parsed model specials. Currently not used.
#' @param ... Currently not used.
#'
#' @return An object of class \code{"SNAIVE2"}.
#' @noRd

train_snaive2 <- function(.data,
                          specials,
                          ...){

  if (length(measured_vars(.data)) > 1) {
    abort("Only univariate responses are supported by SNAIVE2.")
  }

  periods <- common_periods(.data)
  lag_day <- periods["day"]
  lag_week <- periods["week"]

  model_fit <- .data |>
    mutate(
      day_of_week = wday(
        x = !!sym(index_var(.data)),
        week_start = getOption("lubridate.week.start", 1)
      )
    ) |>
    mutate(
      fitted = ifelse(
        day_of_week %in% c(2, 3, 4, 5),
        dplyr::lag(!!sym(measured_vars(.data)), n = lag_day),
        dplyr::lag(!!sym(measured_vars(.data)), n = lag_week)
      )
    ) |>
    mutate(
      resid = !!sym(measured_vars(.data)) - fitted
    )

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


#' @title Forecast a SNAIVE2 model
#'
#' @description
#' Forecast a fitted \code{SNAIVE2} model.
#'
#' @param object A fitted \code{SNAIVE2} model object.
#' @param new_data A \code{tsibble} containing future time points.
#' @param specials Parsed specials. Currently not used.
#' @param ... Additional arguments. Currently not used.
#'
#' @return
#' A vector of forecast distributions.
#'
#' @family SNAIVE2
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tsibble)
#' library(fabletools)
#'
#' train_frame <- elec_price |>
#'   filter(bidding_zone == "DE") |>
#'   slice_head(n = 24 * 21) |>
#'   as_tsibble(index = time)
#'
#' model_frame <- train_frame |>
#'   model("SNAIVE2" = SNAIVE2(value))
#'
#' forecast(model_frame, h = 24)

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
  fcst <- data |>
    append_row(n = n_ahead) |>
    mutate(
      day_of_week = wday(
        x = !!sym(index_var(data)),
        week_start = getOption("lubridate.week.start", 1)
      )
    ) |>
    mutate(
      point = ifelse(
        day_of_week %in% c(2, 3, 4, 5),
        dplyr::lag(!!sym(measured_vars(data)), n = lag_day),
        dplyr::lag(!!sym(measured_vars(data)), n = lag_week)
      )
    ) |>
    slice_tail(n = n_ahead)

  point <- as.numeric(fcst[["point"]])
  sd <- as.numeric(rep(object$sigma, times = n_ahead))

  # Return forecasts
  dist_normal(point, sd)
}


specials_snaive2 <- new_specials()


#' @title Seasonal naive model with weekday-specific lags
#'
#' @description
#' Specify a seasonal naive benchmark model for use with
#' \code{fabletools::model()}.
#'
#' @details
#' \code{SNAIVE2()} is intended for hourly time series. It uses a daily lag for
#' Tuesday to Friday observations and a weekly lag otherwise. This can be useful
#' for electricity price or load data where weekdays have similar intraday
#' structure and weekends require a weekly comparison.
#'
#' @param formula A model formula specifying the response variable, for example
#'   \code{value}.
#' @param ... Further arguments.
#'
#' @return
#' A model definition that can be used inside \code{fabletools::model()}.
#'
#' @family SNAIVE2
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tsibble)
#' library(fabletools)
#'
#' train_frame <- elec_price |>
#'   filter(bidding_zone == "DE") |>
#'   slice_head(n = 24 * 21) |>
#'   as_tsibble(index = time)
#'
#' model_frame <- train_frame |>
#'   model("SNAIVE2" = SNAIVE2(value))
#'
#' model_frame

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


#' @title Extract fitted values from a SNAIVE2 model
#'
#' @description
#' Extract fitted values from a fitted \code{SNAIVE2} model.
#'
#' @param object A fitted \code{SNAIVE2} model object.
#' @param ... Additional arguments. Currently not used.
#'
#' @return
#' Fitted values.
#'
#' @family SNAIVE2
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tsibble)
#' library(fabletools)
#'
#' train_frame <- elec_price |>
#'   filter(bidding_zone == "DE") |>
#'   slice_head(n = 24 * 21) |>
#'   as_tsibble(index = time)
#'
#' model_frame <- train_frame |>
#'   model("SNAIVE2" = SNAIVE2(value))
#'
#' fitted(model_frame)

fitted.SNAIVE2 <- function(object, ...){
  object[["fitted"]]
}


#' @title Extract residuals from a SNAIVE2 model
#'
#' @description
#' Extract residuals from a fitted \code{SNAIVE2} model.
#'
#' @param object A fitted \code{SNAIVE2} model object.
#' @param ... Additional arguments. Currently not used.
#'
#' @return
#' Residuals.
#'
#' @family SNAIVE2
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tsibble)
#' library(fabletools)
#'
#' train_frame <- elec_price |>
#'   filter(bidding_zone == "DE") |>
#'   slice_head(n = 24 * 21) |>
#'   as_tsibble(index = time)
#'
#' model_frame <- train_frame |>
#'   model("SNAIVE2" = SNAIVE2(value))
#'
#' residuals(model_frame)

residuals.SNAIVE2 <- function(object, ...){
  object[["resid"]]
}


#' @title Summarize a SNAIVE2 model
#'
#' @description
#' Return a short model label for a fitted \code{SNAIVE2} model.
#'
#' @param x A fitted \code{SNAIVE2} model object.
#'
#' @return
#' A character string.
#'
#' @family SNAIVE2
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tsibble)
#' library(fabletools)
#'
#' train_frame <- elec_price |>
#'   filter(bidding_zone == "DE") |>
#'   slice_head(n = 24 * 21) |>
#'   as_tsibble(index = time)
#'
#' model_frame <- train_frame |>
#'   model("SNAIVE2" = SNAIVE2(value))
#'
#' model_frame

model_sum.SNAIVE2 <- function(x){
  "SNAIVE2"
}
