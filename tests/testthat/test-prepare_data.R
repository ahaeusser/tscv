
test_that("check_data returns a valid long tsibble", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tsibble")
  skip_if_not_installed("tidyr")

  library(dplyr)
  library(tsibble)
  library(tidyr)

  data <- M4_monthly_data |>
    filter(series %in% c("M23100", "M14395")) |>
    select(index, series, value) |>
    as_tsibble(
      index = index,
      key = series
    )

  checked_data <- check_data(data)

  expect_s3_class(checked_data, "tbl_ts")
  expect_true(is_tsibble(checked_data))
  expect_true(is_regular(checked_data))
  expect_true(is_ordered(checked_data))

  expect_true(all(c("index", "series", "value") %in% names(checked_data)))
  expect_equal(sort(unique(checked_data$series)), c("M14395", "M23100"))
})


test_that("check_data fills implicit missing values", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tsibble")
  skip_if_not_installed("tidyr")

  library(dplyr)
  library(tsibble)
  library(tidyr)

  data <- M4_monthly_data |>
    filter(series == "M23100") |>
    select(index, series, value) |>
    as_tsibble(
      index = index,
      key = series
    )

  data_with_gap <- data[-10, ]

  checked_data <- check_data(
    data = data_with_gap,
    fill_missing = TRUE
  )

  expect_s3_class(checked_data, "tbl_ts")
  expect_equal(nrow(checked_data), nrow(data))
  expect_true(anyNA(checked_data$value))

  missing_row <- checked_data |>
    filter(index == data$index[10])

  expect_equal(nrow(missing_row), 1)
  expect_true(is.na(missing_row$value))
})


test_that("check_data does not fill implicit missing values when fill_missing is FALSE", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tsibble")
  skip_if_not_installed("tidyr")

  library(dplyr)
  library(tsibble)
  library(tidyr)

  data <- M4_monthly_data |>
    filter(series == "M23100") |>
    select(index, series, value) |>
    as_tsibble(
      index = index,
      key = series
    )

  data_with_gap <- data[-10, ]

  checked_data <- check_data(
    data = data_with_gap,
    fill_missing = FALSE
  )

  expect_s3_class(checked_data, "tbl_ts")
  expect_equal(nrow(checked_data), nrow(data_with_gap))
  expect_false(data$index[10] %in% checked_data$index)
})


test_that("check_data converts wide tsibble data to long format", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tsibble")
  skip_if_not_installed("tidyr")

  library(dplyr)
  library(tsibble)
  library(tidyr)

  wide_data <- M4_monthly_data |>
    filter(series %in% c("M23100", "M14395")) |>
    select(index, series, value) |>
    pivot_wider(
      names_from = series,
      values_from = value
    ) |>
    as_tsibble(index = index)

  checked_data <- check_data(wide_data)

  expect_true(all(c("index", "variable", "value") %in% names(checked_data)))
  expect_equal(sort(unique(checked_data$variable)), c("M14395", "M23100"))

  expect_equal(
    nrow(checked_data),
    nrow(wide_data) * 2
  )
})


test_that("check_data errors for non-tsibble input", {
  skip_if_not_installed("dplyr")

  library(dplyr)

  data <- M4_monthly_data |>
    filter(series == "M23100") |>
    select(index, series, value)

  expect_error(
    check_data(data),
    "Please provide a tsibble \\(class tbl_ts\\)."
  )
})


test_that("interpolate_missing replaces missing values", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("forecast")

  library(dplyr)
  library(forecast)

  x <- M4_monthly_data |>
    filter(series == "M23100") |>
    pull(value)

  x_missing <- x
  x_missing[c(10, 20, 30)] <- NA_real_

  x_interpolated <- interpolate_missing(
    x = x_missing,
    periods = 12
  )

  expect_type(x_interpolated, "double")
  expect_equal(length(x_interpolated), length(x_missing))
  expect_false(anyNA(x_interpolated))

  expect_equal(
    x_interpolated[-c(10, 20, 30)],
    x_missing[-c(10, 20, 30)]
  )
})


test_that("interpolate_missing works with hourly seasonal data", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("forecast")

  library(dplyr)
  library(forecast)

  x <- elec_price |>
    filter(bidding_zone == "DE") |>
    slice_head(n = 24 * 21) |>
    pull(value)

  x_missing <- x
  x_missing[c(24, 48, 72)] <- NA_real_

  x_interpolated <- interpolate_missing(
    x = x_missing,
    periods = c(24, 168)
  )

  expect_type(x_interpolated, "double")
  expect_equal(length(x_interpolated), length(x_missing))
  expect_false(anyNA(x_interpolated))
})


test_that("interpolate_missing leaves complete data unchanged", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("forecast")

  library(dplyr)
  library(forecast)

  x <- M4_monthly_data |>
    filter(series == "M23100") |>
    pull(value)

  x_interpolated <- interpolate_missing(
    x = x,
    periods = 12
  )

  expect_equal(x_interpolated, as.numeric(x))
})


test_that("smooth_outlier returns a numeric vector with the same length", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("forecast")

  library(dplyr)
  library(forecast)

  x <- M4_monthly_data |>
    filter(series == "M23100") |>
    pull(value)

  x_smoothed <- smooth_outlier(
    x = x,
    periods = 12
  )

  expect_type(x_smoothed, "double")
  expect_equal(length(x_smoothed), length(x))
  expect_false(anyNA(x_smoothed))
})


test_that("smooth_outlier replaces a clear injected outlier", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("forecast")

  library(dplyr)
  library(forecast)

  x <- M4_monthly_data |>
    filter(series == "M23100") |>
    pull(value)

  x_outlier <- x
  x_outlier[20] <- max(x, na.rm = TRUE) * 100

  x_smoothed <- smooth_outlier(
    x = x_outlier,
    periods = 12
  )

  expect_type(x_smoothed, "double")
  expect_equal(length(x_smoothed), length(x_outlier))
  expect_false(anyNA(x_smoothed))

  expect_lt(
    abs(x_smoothed[20] - x[20]),
    abs(x_outlier[20] - x[20])
  )
})


test_that("smooth_outlier works with hourly seasonal data", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("forecast")

  library(dplyr)
  library(forecast)

  x <- elec_price |>
    filter(bidding_zone == "DE") |>
    slice_head(n = 24 * 21) |>
    pull(value)

  x_outlier <- x
  x_outlier[48] <- max(abs(x), na.rm = TRUE) * 100

  x_smoothed <- smooth_outlier(
    x = x_outlier,
    periods = c(24, 168)
  )

  expect_type(x_smoothed, "double")
  expect_equal(length(x_smoothed), length(x_outlier))
  expect_false(anyNA(x_smoothed))
})
