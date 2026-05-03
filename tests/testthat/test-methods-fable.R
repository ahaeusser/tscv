
make_monthly_train_frame <- function() {
  M4_monthly_data |>
    dplyr::filter(series == dplyr::first(series)) |>
    tsibble::as_tsibble(index = index)
}

make_hourly_train_frame <- function(n_days = 21) {
  elec_price |>
    dplyr::filter(bidding_zone == "DE") |>
    dplyr::slice_head(n = 24 * n_days) |>
    tsibble::as_tsibble(index = time)
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
}

expect_valid_fitted <- function(x, n_obs, n_models = 1) {
  expect_s3_class(x, "tbl_ts")
  expect_equal(nrow(x), n_obs * n_models)
  expect_true(".fitted" %in% names(x))
}

expect_valid_residuals <- function(x, n_obs, n_models = 1) {
  expect_s3_class(x, "tbl_ts")
  expect_equal(nrow(x), n_obs * n_models)
  expect_true(".resid" %in% names(x))
}


test_that("monthly and hourly example train frames are valid", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tsibble")

  monthly_train_frame <- make_monthly_train_frame()
  hourly_train_frame <- make_hourly_train_frame()

  expect_s3_class(monthly_train_frame, "tbl_ts")
  expect_s3_class(hourly_train_frame, "tbl_ts")

  expect_true("value" %in% names(monthly_train_frame))
  expect_true("value" %in% names(hourly_train_frame))

  expect_gt(nrow(monthly_train_frame), 12)
  expect_equal(nrow(hourly_train_frame), 24 * 21)
})


test_that("MEDIAN works via fabletools::model, forecast, fitted, and residuals", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tsibble")
  skip_if_not_installed("fabletools")
  skip_if_not_installed("distributional")

  train_frame <- make_monthly_train_frame()

  model_frame <- train_frame |>
    fabletools::model("MEDIAN" = MEDIAN(value ~ window()))

  expect_valid_mable(model_frame, "MEDIAN")

  fc <- fabletools::forecast(model_frame, h = 12)
  fit <- fitted(model_frame)
  res <- residuals(model_frame)

  expect_valid_forecast(fc, h = 12)
  expect_valid_fitted(fit, n_obs = nrow(train_frame))
  expect_valid_residuals(res, n_obs = nrow(train_frame))
})


test_that("MEDIAN fixed window works via fabletools::model", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tsibble")
  skip_if_not_installed("fabletools")
  skip_if_not_installed("distributional")

  train_frame <- make_monthly_train_frame()

  model_frame <- train_frame |>
    fabletools::model("MEDIAN" = MEDIAN(value ~ window(size = 12)))

  expect_valid_mable(model_frame, "MEDIAN")

  fc <- fabletools::forecast(model_frame, h = 12)
  fit <- fitted(model_frame)
  res <- residuals(model_frame)

  expect_valid_forecast(fc, h = 12)
  expect_valid_fitted(fit, n_obs = nrow(train_frame))
  expect_valid_residuals(res, n_obs = nrow(train_frame))
})


test_that("SMEAN works via fabletools::model, forecast, fitted, and residuals", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tsibble")
  skip_if_not_installed("fabletools")
  skip_if_not_installed("distributional")

  train_frame <- make_monthly_train_frame()

  model_frame <- train_frame |>
    fabletools::model("SMEAN" = SMEAN(value ~ lag("year")))

  expect_valid_mable(model_frame, "SMEAN")

  fc <- fabletools::forecast(model_frame, h = 12)
  fit <- fitted(model_frame)
  res <- residuals(model_frame)

  expect_valid_forecast(fc, h = 12)
  expect_valid_fitted(fit, n_obs = nrow(train_frame))
  expect_valid_residuals(res, n_obs = nrow(train_frame))
})


test_that("SMEDIAN works with monthly yearly seasonality", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tsibble")
  skip_if_not_installed("fabletools")
  skip_if_not_installed("distributional")

  train_frame <- make_monthly_train_frame()

  model_frame <- train_frame |>
    fabletools::model("SMEDIAN" = SMEDIAN(value ~ lag("year")))

  expect_valid_mable(model_frame, "SMEDIAN")

  fc <- fabletools::forecast(model_frame, h = 12)
  fit <- fitted(model_frame)
  res <- residuals(model_frame)

  expect_valid_forecast(fc, h = 12)
  expect_valid_fitted(fit, n_obs = nrow(train_frame))
  expect_valid_residuals(res, n_obs = nrow(train_frame))
})


test_that("SMEDIAN works with hourly weekly seasonality", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tsibble")
  skip_if_not_installed("fabletools")
  skip_if_not_installed("distributional")

  train_frame <- make_hourly_train_frame(n_days = 21)

  model_frame <- train_frame |>
    fabletools::model("SMEDIAN" = SMEDIAN(value ~ lag("week")))

  expect_valid_mable(model_frame, "SMEDIAN")

  fc <- fabletools::forecast(model_frame, h = 24)
  fit <- fitted(model_frame)
  res <- residuals(model_frame)

  expect_valid_forecast(fc, h = 24)
  expect_valid_fitted(fit, n_obs = nrow(train_frame))
  expect_valid_residuals(res, n_obs = nrow(train_frame))
})


test_that("SNAIVE2 works via fabletools::model, forecast, fitted, and residuals", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tsibble")
  skip_if_not_installed("fabletools")
  skip_if_not_installed("lubridate")
  skip_if_not_installed("distributional")

  train_frame <- make_hourly_train_frame(n_days = 21)

  model_frame <- train_frame |>
    fabletools::model("SNAIVE2" = SNAIVE2(value))

  expect_valid_mable(model_frame, "SNAIVE2")

  fc <- fabletools::forecast(model_frame, h = 24)
  fit <- fitted(model_frame)
  res <- residuals(model_frame)

  expect_valid_forecast(fc, h = 24)
  expect_valid_fitted(fit, n_obs = nrow(train_frame))
  expect_valid_residuals(res, n_obs = nrow(train_frame))
})


test_that("SNAIVE2 and SMEDIAN work together in the documented hourly workflow", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tsibble")
  skip_if_not_installed("fabletools")
  skip_if_not_installed("lubridate")
  skip_if_not_installed("distributional")

  train_frame <- make_hourly_train_frame(n_days = 21)

  model_frame <- train_frame |>
    fabletools::model(
      "SNAIVE2" = SNAIVE2(value),
      "SMEDIAN" = SMEDIAN(value ~ lag("week"))
    )

  expect_s3_class(model_frame, "mdl_df")
  expect_equal(nrow(model_frame), 1)
  expect_true("SNAIVE2" %in% names(model_frame))
  expect_true("SMEDIAN" %in% names(model_frame))

  fc <- fabletools::forecast(model_frame, h = 24)
  fit <- fitted(model_frame)
  res <- residuals(model_frame)

  expect_valid_forecast(fc, h = 24, n_models = 2)
  expect_valid_fitted(fit, n_obs = nrow(train_frame), n_models = 2)
  expect_valid_residuals(res, n_obs = nrow(train_frame), n_models = 2)
})


test_that("TBATS works via fabletools::model, forecast, fitted, and residuals", {
  skip_on_cran()
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tsibble")
  skip_if_not_installed("fabletools")
  skip_if_not_installed("forecast")
  skip_if_not_installed("distributional")

  train_frame <- make_hourly_train_frame(n_days = 21)

  model_frame <- train_frame |>
    fabletools::model("TBATS" = TBATS(value, periods = c(24, 168)))

  expect_valid_mable(model_frame, "TBATS")

  fc <- fabletools::forecast(model_frame, h = 24)
  fit <- fitted(model_frame)
  res <- residuals(model_frame)

  expect_valid_forecast(fc, h = 24)
  expect_valid_fitted(fit, n_obs = nrow(train_frame))
  expect_valid_residuals(res, n_obs = nrow(train_frame))
})


test_that("DSHW works via fabletools::model, forecast, fitted, and residuals", {
  skip_on_cran()
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tsibble")
  skip_if_not_installed("fabletools")
  skip_if_not_installed("forecast")
  skip_if_not_installed("distributional")

  train_frame <- make_hourly_train_frame(n_days = 28)

  # avoid negative values with DSHW
  model_frame <- train_frame |>
    fabletools::model("DSHW" = DSHW(value + 200, periods = c(24, 168)))

  expect_valid_mable(model_frame, "DSHW")

  fc <- fabletools::forecast(model_frame, h = 24)
  fit <- fitted(model_frame)
  res <- residuals(model_frame)

  expect_valid_forecast(fc, h = 24)
  expect_valid_fitted(fit, n_obs = nrow(train_frame))
  expect_valid_residuals(res, n_obs = nrow(train_frame))
})


test_that("TBATS and DSHW work together in a mable", {
  skip_on_cran()
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tsibble")
  skip_if_not_installed("fabletools")
  skip_if_not_installed("forecast")
  skip_if_not_installed("distributional")

  train_frame <- make_hourly_train_frame(n_days = 28)

  # avoid negative values with DSHW
  model_frame <- train_frame |>
    fabletools::model(
      "TBATS" = TBATS(value, periods = c(24, 168)),
      "DSHW" = DSHW(value + 200, periods = c(24, 168))
    )

  expect_s3_class(model_frame, "mdl_df")
  expect_equal(nrow(model_frame), 1)
  expect_true("TBATS" %in% names(model_frame))
  expect_true("DSHW" %in% names(model_frame))

  fc <- fabletools::forecast(model_frame, h = 24)
  fit <- fitted(model_frame)
  res <- residuals(model_frame)

  expect_valid_forecast(fc, h = 24, n_models = 2)
  expect_valid_fitted(fit, n_obs = nrow(train_frame), n_models = 2)
  expect_valid_residuals(res, n_obs = nrow(train_frame), n_models = 2)
})


test_that("benchmark models can be fitted together on monthly data", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tsibble")
  skip_if_not_installed("fabletools")
  skip_if_not_installed("distributional")

  train_frame <- make_monthly_train_frame()

  model_frame <- train_frame |>
    fabletools::model(
      "MEDIAN" = MEDIAN(value ~ window()),
      "SMEAN" = SMEAN(value ~ lag("year")),
      "SMEDIAN" = SMEDIAN(value ~ lag("year"))
    )

  expect_s3_class(model_frame, "mdl_df")
  expect_equal(nrow(model_frame), 1)
  expect_true("MEDIAN" %in% names(model_frame))
  expect_true("SMEAN" %in% names(model_frame))
  expect_true("SMEDIAN" %in% names(model_frame))

  fc <- fabletools::forecast(model_frame, h = 12)
  fit <- fitted(model_frame)
  res <- residuals(model_frame)

  expect_valid_forecast(fc, h = 12, n_models = 3)
  expect_valid_fitted(fit, n_obs = nrow(train_frame), n_models = 3)
  expect_valid_residuals(res, n_obs = nrow(train_frame), n_models = 3)
})
