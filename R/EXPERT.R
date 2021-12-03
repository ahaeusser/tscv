
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

  series_id <- measured_vars(.data)
  index_id <- index_var(.data)

  # Prepare data set (convert to tibble and add date-time columns)
  data <- .data %>%
    append_row(n = min(periods)) %>%
    as_tibble() %>%
    mutate(ymd = date(!!sym(index_id))) %>%
    mutate(wday = wday(!!sym(index_id))) %>%
    mutate(hour = hour(!!sym(index_id)))

  # Create lagged features
  data <- data %>%
    mutate("lag(1)" = dplyr::lag(!!sym(series_id), n = 1 * min(periods))) %>%  # (d-1)
    mutate("lag(2)" = dplyr::lag(!!sym(series_id), n = 2 * min(periods))) %>%  # (d-2)
    mutate("lag(7)" = dplyr::lag(!!sym(series_id), n = 7 * min(periods)))      # (d-7)

  # Drop leading missing values
  data <- data %>%
    slice(n = -c(1:max(periods)))

  # Create minimum and maximum of previous day
  data <- data %>%
    group_by(ymd) %>%
    mutate("min(1)" = min(.data$`lag(1)`, na.rm = TRUE)) %>%  # min(d-1)
    mutate("max(1)" = max(.data$`lag(1)`, na.rm = TRUE)) %>%  # max(d-1)
    ungroup()

  # Create "midnight" feature
  data <- data %>%
    mutate(midnight = ifelse(hour == 0, `lag(1)`, NA_real_)) %>%
    fill(midnight, .direction = "down")

  # Create "midnight" feature
  # Special treatment for h == 0
  data <- data %>%
    mutate(midnight = ifelse(hour == 0, `lag(1)`, NA_real_)) %>%
    fill(midnight, .direction = "down")

  # Create day-of-week dummy variables
  # Sun = 1, Mon = 2, ..., Sat = 7
  data <- data %>%
    mutate("mon" = ifelse(wday == 2, 1, 0)) %>%
    mutate("sat" = ifelse(wday == 7, 1, 0)) %>%
    mutate("sun" = ifelse(wday == 1, 1, 0))

  # Row indices for training and forecasting
  idx_total <- 1:nrow(data)
  idx_test <- tail(idx_total, n = min(periods))
  idx_train <- idx_total[-idx_test]

  # Remove "helper" variables
  data <- data %>%
    select(-c(!!sym(index_id), ymd, wday))

  # Prepare training data .......................................................
  data_train <- data %>%
    slice(idx_train) %>%
    group_by(hour) %>%
    group_split(.keep = FALSE) %>%
    as.list()

  # Prepare testing data ........................................................
  # (for out-of-sample forecasts)

  data_test <- data %>%
    slice(idx_test) %>%
    group_by(hour) %>%
    group_split(.keep = FALSE) %>%
    as.list()

  # Special treatment:
  # Predictor "midnight" is excluded when hour == 0 due to multicollinearity

  data_train[[1]]["midnight"] <- NULL
  data_test[[1]]["midnight"] <- NULL

  # Train linear models via OLS
  formula <- as.formula(paste0(series_id, "~ ."))

  models_fit <- map(
    .x = data_train,
    .f = ~lm(
      formula = formula,
      data = .
    )
  )

  # models_fit[[.x]]$residuals
  # models_fit[[.x]]$fitted.values

  # Extract fitted values and residuals
  fitted <- NULL
  resid <- NULL
  sigma <- NULL

  # Return model
  structure(
    list(
      model = models_fit,
      test = data_test,
      fitted = fitted,
      resid = resid,
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
  point <- map_dbl(
    .x = 1:length(object[["model"]]),
    .f = ~{
      predict(
        object = object[["model"]][[.x]],
        newdata = object[["test"]][[.x]]
      )
    }
  )

  sd <- rep(NA_real_, length(point))

  # Return forecasts
  dist_normal(point, sd)
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
  object[["fitted"]]
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
  object[["resid"]]
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
