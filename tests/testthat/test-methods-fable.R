
make_monthly_train_frame <- function() {
  M4_monthly_data |>
    filter(series == first(series)) |>
    as_tsibble(index = index)
}


make_price_hourly_train_frame <- function(n_days = 21) {
  elec_price |>
    filter(bidding_zone == "DE") |>
    slice_head(n = 24 * n_days) |>
    as_tsibble(index = time)
}


make_load_hourly_train_frame <- function(n_days = 28) {
  elec_load |>
    filter(bidding_zone == "DE") |>
    slice_head(n = 24 * n_days) |>
    as_tsibble(index = time)
}


expect_valid_mable <- function(x, model_name) {
  expect_s3_class(x, "mdl_df")
  expect_true(model_name %in% names(x))
  expect_equal(nrow(x), 1)
}


expect_valid_forecast <- function(x, h, n_models = 1) {
  expect_s3_class(x, "fbl_ts")
  expect_equal(nrow(x), h * n_models)
  expect_true(".mean" %in% names(x))
  expect_true(all(is.finite(x$.mean)))
}


expect_valid_fitted <- function(x, n_obs, n_models = 1) {
  expect_s3_class(x, "tbl_ts")
  expect_equal(nrow(x), n_obs * n_models)
  expect_true(".fitted" %in% names(x))
  expect_true(any(is.finite(x$.fitted)))
}


expect_valid_residuals <- function(x, n_obs, n_models = 1) {
  expect_s3_class(x, "tbl_ts")
  expect_equal(nrow(x), n_obs * n_models)
  expect_true(".resid" %in% names(x))
  expect_true(any(is.finite(x$.resid)))
}


test_that("example training frames are valid", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tsibble")

  library(dplyr)
  library(tsibble)

  monthly_train_frame <- make_monthly_train_frame()
  price_train_frame <- make_price_hourly_train_frame(n_days = 21)
  load_train_frame <- make_load_hourly_train_frame(n_days = 28)

  expect_s3_class(monthly_train_frame, "tbl_ts")
  expect_s3_class(price_train_frame, "tbl_ts")
  expect_s3_class(load_train_frame, "tbl_ts")

  expect_true("value" %in% names(monthly_train_frame))
  expect_true("value" %in% names(price_train_frame))
  expect_true("value" %in% names(load_train_frame))

  expect_gt(nrow(monthly_train_frame), 12)
  expect_equal(nrow(price_train_frame), 24 * 21)
  expect_equal(nrow(load_train_frame), 24 * 28)

  expect_true(all(load_train_frame$value > 0, na.rm = TRUE))
})


test_that("MEDIAN works with all observations", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tsibble")
  skip_if_not_installed("fabletools")
  skip_if_not_installed("distributional")

  library(dplyr)
  library(tsibble)
  library(fabletools)

  train_frame <- make_monthly_train_frame()

  model_frame <- train_frame |>
    model("MEDIAN" = MEDIAN(value ~ window()))

  expect_valid_mable(model_frame, "MEDIAN")

  fc <- forecast(model_frame, h = 12)
  fit <- fitted(model_frame)
  res <- residuals(model_frame)

  expect_valid_forecast(fc, h = 12)
  expect_valid_fitted(fit, n_obs = nrow(train_frame))
  expect_valid_residuals(res, n_obs = nrow(train_frame))
})


test_that("MEDIAN works with a fixed window", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tsibble")
  skip_if_not_installed("fabletools")
  skip_if_not_installed("distributional")

  library(dplyr)
  library(tsibble)
  library(fabletools)

  train_frame <- make_monthly_train_frame()

  model_frame <- train_frame |>
    model("MEDIAN" = MEDIAN(value ~ window(size = 12)))

  expect_valid_mable(model_frame, "MEDIAN")

  fc <- forecast(model_frame, h = 12)
  fit <- fitted(model_frame)
  res <- residuals(model_frame)

  expect_valid_forecast(fc, h = 12)
  expect_valid_fitted(fit, n_obs = nrow(train_frame))
  expect_valid_residuals(res, n_obs = nrow(train_frame))
})


test_that("SMEAN works with yearly seasonality", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tsibble")
  skip_if_not_installed("fabletools")
  skip_if_not_installed("distributional")

  library(dplyr)
  library(tsibble)
  library(fabletools)

  train_frame <- make_monthly_train_frame()

  model_frame <- train_frame |>
    model("SMEAN" = SMEAN(value ~ lag("year")))

  expect_valid_mable(model_frame, "SMEAN")

  fc <- forecast(model_frame, h = 12)
  fit <- fitted(model_frame)
  res <- residuals(model_frame)

  expect_valid_forecast(fc, h = 12)
  expect_valid_fitted(fit, n_obs = nrow(train_frame))
  expect_valid_residuals(res, n_obs = nrow(train_frame))
})


test_that("SMEDIAN works with yearly seasonality", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tsibble")
  skip_if_not_installed("fabletools")
  skip_if_not_installed("distributional")

  library(dplyr)
  library(tsibble)
  library(fabletools)

  train_frame <- make_monthly_train_frame()

  model_frame <- train_frame |>
    model("SMEDIAN" = SMEDIAN(value ~ lag("year")))

  expect_valid_mable(model_frame, "SMEDIAN")

  fc <- forecast(model_frame, h = 12)
  fit <- fitted(model_frame)
  res <- residuals(model_frame)

  expect_valid_forecast(fc, h = 12)
  expect_valid_fitted(fit, n_obs = nrow(train_frame))
  expect_valid_residuals(res, n_obs = nrow(train_frame))
})


test_that("SMEDIAN works with weekly seasonality", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tsibble")
  skip_if_not_installed("fabletools")
  skip_if_not_installed("distributional")

  library(dplyr)
  library(tsibble)
  library(fabletools)

  train_frame <- make_price_hourly_train_frame(n_days = 21)

  model_frame <- train_frame |>
    model("SMEDIAN" = SMEDIAN(value ~ lag("week")))

  expect_valid_mable(model_frame, "SMEDIAN")

  fc <- forecast(model_frame, h = 24)
  fit <- fitted(model_frame)
  res <- residuals(model_frame)

  expect_valid_forecast(fc, h = 24)
  expect_valid_fitted(fit, n_obs = nrow(train_frame))
  expect_valid_residuals(res, n_obs = nrow(train_frame))
})


test_that("SNAIVE2 works with hourly price data", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tsibble")
  skip_if_not_installed("fabletools")
  skip_if_not_installed("lubridate")
  skip_if_not_installed("distributional")

  library(dplyr)
  library(tsibble)
  library(fabletools)

  train_frame <- make_price_hourly_train_frame(n_days = 21)

  model_frame <- train_frame |>
    model("SNAIVE2" = SNAIVE2(value))

  expect_valid_mable(model_frame, "SNAIVE2")

  fc <- forecast(model_frame, h = 24)
  fit <- fitted(model_frame)
  res <- residuals(model_frame)

  expect_valid_forecast(fc, h = 24)
  expect_valid_fitted(fit, n_obs = nrow(train_frame))
  expect_valid_residuals(res, n_obs = nrow(train_frame))
})


test_that("benchmark models work together on monthly data", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tsibble")
  skip_if_not_installed("fabletools")
  skip_if_not_installed("distributional")

  library(dplyr)
  library(tsibble)
  library(fabletools)

  train_frame <- make_monthly_train_frame()

  model_frame <- train_frame |>
    model(
      "MEDIAN" = MEDIAN(value ~ window()),
      "SMEAN" = SMEAN(value ~ lag("year")),
      "SMEDIAN" = SMEDIAN(value ~ lag("year"))
    )

  expect_s3_class(model_frame, "mdl_df")
  expect_equal(nrow(model_frame), 1)
  expect_true("MEDIAN" %in% names(model_frame))
  expect_true("SMEAN" %in% names(model_frame))
  expect_true("SMEDIAN" %in% names(model_frame))

  fc <- forecast(model_frame, h = 12)
  fit <- fitted(model_frame)
  res <- residuals(model_frame)

  expect_valid_forecast(fc, h = 12, n_models = 3)
  expect_valid_fitted(fit, n_obs = nrow(train_frame), n_models = 3)
  expect_valid_residuals(res, n_obs = nrow(train_frame), n_models = 3)
})


test_that("SNAIVE2 and SMEDIAN work together on hourly price data", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tsibble")
  skip_if_not_installed("fabletools")
  skip_if_not_installed("lubridate")
  skip_if_not_installed("distributional")

  library(dplyr)
  library(tsibble)
  library(fabletools)

  train_frame <- make_price_hourly_train_frame(n_days = 21)

  model_frame <- train_frame |>
    model(
      "SNAIVE2" = SNAIVE2(value),
      "SMEDIAN" = SMEDIAN(value ~ lag("week"))
    )

  expect_s3_class(model_frame, "mdl_df")
  expect_equal(nrow(model_frame), 1)
  expect_true("SNAIVE2" %in% names(model_frame))
  expect_true("SMEDIAN" %in% names(model_frame))

  fc <- forecast(model_frame, h = 24)
  fit <- fitted(model_frame)
  res <- residuals(model_frame)

  expect_valid_forecast(fc, h = 24, n_models = 2)
  expect_valid_fitted(fit, n_obs = nrow(train_frame), n_models = 2)
  expect_valid_residuals(res, n_obs = nrow(train_frame), n_models = 2)
})


test_that("TBATS works with hourly price data", {
  skip_on_cran()
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tsibble")
  skip_if_not_installed("fabletools")
  skip_if_not_installed("forecast")
  skip_if_not_installed("distributional")

  library(dplyr)
  library(tsibble)
  library(fabletools)

  train_frame <- make_price_hourly_train_frame(n_days = 21)

  model_frame <- train_frame |>
    model("TBATS" = TBATS(value, periods = c(24, 168)))

  expect_valid_mable(model_frame, "TBATS")

  fc <- forecast(model_frame, h = 24)
  fit <- fitted(model_frame)
  res <- residuals(model_frame)

  expect_valid_forecast(fc, h = 24)
  expect_valid_fitted(fit, n_obs = nrow(train_frame))
  expect_valid_residuals(res, n_obs = nrow(train_frame))
})


test_that("DSHW works with hourly load data", {
  skip_on_cran()
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tsibble")
  skip_if_not_installed("fabletools")
  skip_if_not_installed("forecast")
  skip_if_not_installed("distributional")

  library(dplyr)
  library(tsibble)
  library(fabletools)

  train_frame <- make_price_hourly_train_frame(n_days = 28)

  model_frame <- train_frame |>
    model("DSHW" = DSHW(value + 200, periods = c(24, 168)))

  expect_valid_mable(model_frame, "DSHW")

  fc <- forecast(model_frame, h = 24)
  fit <- fitted(model_frame)
  res <- residuals(model_frame)

  expect_valid_forecast(fc, h = 24)
  expect_valid_fitted(fit, n_obs = nrow(train_frame))
  expect_valid_residuals(res, n_obs = nrow(train_frame))
})


test_that("TBATS and DSHW work together on hourly load data", {
  skip_on_cran()
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tsibble")
  skip_if_not_installed("fabletools")
  skip_if_not_installed("forecast")
  skip_if_not_installed("distributional")

  library(dplyr)
  library(tsibble)
  library(fabletools)

  train_frame <- make_price_hourly_train_frame(n_days = 28)

  model_frame <- train_frame |>
    model(
      "TBATS" = TBATS(value, periods = c(24, 168)),
      "DSHW" = DSHW(value + 200, periods = c(24, 168))
    )

  expect_s3_class(model_frame, "mdl_df")
  expect_equal(nrow(model_frame), 1)
  expect_true("TBATS" %in% names(model_frame))
  expect_true("DSHW" %in% names(model_frame))

  fc <- forecast(model_frame, h = 24)
  fit <- fitted(model_frame)
  res <- residuals(model_frame)

  expect_valid_forecast(fc, h = 24, n_models = 2)
  expect_valid_fitted(fit, n_obs = nrow(train_frame), n_models = 2)
  expect_valid_residuals(res, n_obs = nrow(train_frame), n_models = 2)
})
