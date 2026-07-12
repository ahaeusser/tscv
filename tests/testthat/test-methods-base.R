
test_that("forecast_naive2 repeats the final value for a non-seasonal series", {
  x <- 1:20

  forecast <- forecast_naive2(
    x = x,
    freq = 1,
    n_ahead = 5
  )

  expect_equal(forecast, rep(20, 5))
})


test_that("forecast_naive2 returns seasonal forecasts for a seasonal series", {
  x <- as.numeric(AirPassengers)

  forecast <- forecast_naive2(
    x = x,
    freq = 12,
    n_ahead = 12
  )

  decomposition <- decompose(
    x = ts(x, frequency = 12),
    type = "multiplicative"
  )

  seasonal_factors <- as.numeric(decomposition$seasonal)
  adjusted_x <- x / seasonal_factors

  expected <- tail(adjusted_x, 1) *
    tail(seasonal_factors, 12)

  expect_equal(forecast, expected)
})


test_that("forecast_naive2 returns the requested number of forecasts", {
  x <- as.numeric(AirPassengers)

  forecast <- forecast_naive2(
    x = x,
    freq = 12,
    n_ahead = 18
  )

  expect_length(forecast, 18)
  expect_type(forecast, "double")
})
