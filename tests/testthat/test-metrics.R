
test_that("me_vec calculates mean error", {
  truth <- c(10, 20, 30)
  estimate <- c(8, 22, 25)

  result <- me_vec(truth, estimate)

  expect_type(result, "double")
  expect_length(result, 1)
  expect_equal(result, mean(truth - estimate))
  expect_equal(result, (2 - 2 + 5) / 3)
})


test_that("mae_vec calculates mean absolute error", {
  truth <- c(10, 20, 30)
  estimate <- c(8, 22, 25)

  result <- mae_vec(truth, estimate)

  expect_type(result, "double")
  expect_length(result, 1)
  expect_equal(result, mean(abs(truth - estimate)))
  expect_equal(result, (2 + 2 + 5) / 3)
})


test_that("mse_vec calculates mean squared error", {
  truth <- c(10, 20, 30)
  estimate <- c(8, 22, 25)

  result <- mse_vec(truth, estimate)

  expect_type(result, "double")
  expect_length(result, 1)
  expect_equal(result, mean((truth - estimate)^2))
  expect_equal(result, (4 + 4 + 25) / 3)
})


test_that("rmse_vec calculates root mean squared error", {
  truth <- c(10, 20, 30)
  estimate <- c(8, 22, 25)

  result <- rmse_vec(truth, estimate)

  expect_type(result, "double")
  expect_length(result, 1)
  expect_equal(result, sqrt(mean((truth - estimate)^2)))
  expect_equal(result, sqrt((4 + 4 + 25) / 3))
})


test_that("mpe_vec calculates mean percentage error", {
  truth <- c(10, 20, 40)
  estimate <- c(8, 22, 30)

  expected <- mean(((truth - estimate) / truth) * 100)

  result <- mpe_vec(truth, estimate)

  expect_type(result, "double")
  expect_length(result, 1)
  expect_equal(result, expected)
  expect_equal(result, mean(c(20, -10, 25)))
})


test_that("mape_vec calculates mean absolute percentage error", {
  truth <- c(10, 20, 40)
  estimate <- c(8, 22, 30)

  expected <- mean(abs((truth - estimate) / truth)) * 100

  result <- mape_vec(truth, estimate)

  expect_type(result, "double")
  expect_length(result, 1)
  expect_equal(result, expected)
  expect_equal(result, mean(c(20, 10, 25)))
})


test_that("smape_vec calculates symmetric mean absolute percentage error", {
  truth <- c(10, 20, 40)
  estimate <- c(8, 22, 30)

  expected <- mean(
    abs(estimate - truth) /
      ((abs(truth) + abs(estimate)) / 2)
  ) * 100

  result <- smape_vec(truth, estimate)

  expect_type(result, "double")
  expect_length(result, 1)
  expect_equal(result, expected)
})


test_that("all error metrics return zero for perfect forecasts", {
  truth <- c(10, 20, 30)
  estimate <- truth

  expect_equal(me_vec(truth, estimate), 0)
  expect_equal(mae_vec(truth, estimate), 0)
  expect_equal(mse_vec(truth, estimate), 0)
  expect_equal(rmse_vec(truth, estimate), 0)
  expect_equal(mpe_vec(truth, estimate), 0)
  expect_equal(mape_vec(truth, estimate), 0)
  expect_equal(smape_vec(truth, estimate), 0)
})


test_that("absolute and squared metrics are non-negative", {
  truth <- c(10, 20, 30, 40)
  estimate <- c(8, 22, 35, 39)

  expect_gte(mae_vec(truth, estimate), 0)
  expect_gte(mse_vec(truth, estimate), 0)
  expect_gte(rmse_vec(truth, estimate), 0)
  expect_gte(mape_vec(truth, estimate), 0)
  expect_gte(smape_vec(truth, estimate), 0)
})


test_that("rmse_vec is the square root of mse_vec", {
  truth <- c(10, 20, 30, 40)
  estimate <- c(8, 22, 35, 39)

  expect_equal(
    rmse_vec(truth, estimate),
    sqrt(mse_vec(truth, estimate))
  )
})


test_that("metrics remove missing values by default", {
  truth <- c(10, 20, 30, NA)
  estimate <- c(8, 22, 25, 100)

  complete_truth <- c(10, 20, 30)
  complete_estimate <- c(8, 22, 25)

  expect_equal(
    me_vec(truth, estimate),
    me_vec(complete_truth, complete_estimate)
  )

  expect_equal(
    mae_vec(truth, estimate),
    mae_vec(complete_truth, complete_estimate)
  )

  expect_equal(
    mse_vec(truth, estimate),
    mse_vec(complete_truth, complete_estimate)
  )

  expect_equal(
    rmse_vec(truth, estimate),
    rmse_vec(complete_truth, complete_estimate)
  )

  expect_equal(
    mpe_vec(truth, estimate),
    mpe_vec(complete_truth, complete_estimate)
  )

  expect_equal(
    mape_vec(truth, estimate),
    mape_vec(complete_truth, complete_estimate)
  )

  expect_equal(
    smape_vec(truth, estimate),
    smape_vec(complete_truth, complete_estimate)
  )
})


test_that("metrics return NA when na_rm = FALSE and inputs contain NA", {
  truth <- c(10, 20, 30, NA)
  estimate <- c(8, 22, 25, 100)

  expect_true(is.na(me_vec(truth, estimate, na_rm = FALSE)))
  expect_true(is.na(mae_vec(truth, estimate, na_rm = FALSE)))
  expect_true(is.na(mse_vec(truth, estimate, na_rm = FALSE)))
  expect_true(is.na(rmse_vec(truth, estimate, na_rm = FALSE)))
  expect_true(is.na(mpe_vec(truth, estimate, na_rm = FALSE)))
  expect_true(is.na(mape_vec(truth, estimate, na_rm = FALSE)))
  expect_true(is.na(smape_vec(truth, estimate, na_rm = FALSE)))
})


test_that("metrics handle missing values in estimate by default", {
  truth <- c(10, 20, 30, 40)
  estimate <- c(8, NA, 25, 45)

  complete_truth <- c(10, 30, 40)
  complete_estimate <- c(8, 25, 45)

  expect_equal(
    me_vec(truth, estimate),
    me_vec(complete_truth, complete_estimate)
  )

  expect_equal(
    mae_vec(truth, estimate),
    mae_vec(complete_truth, complete_estimate)
  )

  expect_equal(
    mse_vec(truth, estimate),
    mse_vec(complete_truth, complete_estimate)
  )

  expect_equal(
    rmse_vec(truth, estimate),
    rmse_vec(complete_truth, complete_estimate)
  )

  expect_equal(
    mpe_vec(truth, estimate),
    mpe_vec(complete_truth, complete_estimate)
  )

  expect_equal(
    mape_vec(truth, estimate),
    mape_vec(complete_truth, complete_estimate)
  )

  expect_equal(
    smape_vec(truth, estimate),
    smape_vec(complete_truth, complete_estimate)
  )
})


test_that("percentage metrics return Inf when truth contains zero and estimate differs", {
  truth <- c(0, 10)
  estimate <- c(1, 8)

  expect_true(is.infinite(mpe_vec(truth, estimate)))
  expect_true(is.infinite(mape_vec(truth, estimate)))
})


test_that("percentage metrics return NaN when truth and estimate are both zero", {
  truth <- c(0)
  estimate <- c(0)

  expect_true(is.nan(mpe_vec(truth, estimate)))
  expect_true(is.nan(mape_vec(truth, estimate)))
  expect_true(is.nan(smape_vec(truth, estimate)))
})


test_that("smape_vec handles negative values using absolute denominator", {
  truth <- c(-10, 10)
  estimate <- c(-8, 8)

  expected <- mean(
    abs(estimate - truth) /
      ((abs(truth) + abs(estimate)) / 2)
  ) * 100

  expect_equal(smape_vec(truth, estimate), expected)
  expect_true(is.finite(smape_vec(truth, estimate)))
})


test_that("metrics return NaN for empty numeric vectors", {
  truth <- numeric()
  estimate <- numeric()

  expect_true(is.nan(me_vec(truth, estimate)))
  expect_true(is.nan(mae_vec(truth, estimate)))
  expect_true(is.nan(mse_vec(truth, estimate)))
  expect_true(is.nan(rmse_vec(truth, estimate)))
  expect_true(is.nan(mpe_vec(truth, estimate)))
  expect_true(is.nan(mape_vec(truth, estimate)))
  expect_true(is.nan(smape_vec(truth, estimate)))
})


test_that("metrics recycle vectors according to base R arithmetic rules", {
  truth <- c(10, 20, 30, 40)
  estimate <- c(5, 10)

  expect_equal(
    me_vec(truth, estimate),
    mean(truth - estimate)
  )

  expect_equal(
    mae_vec(truth, estimate),
    mean(abs(truth - estimate))
  )

  expect_equal(
    mse_vec(truth, estimate),
    mean((truth - estimate)^2)
  )

  expect_equal(
    rmse_vec(truth, estimate),
    sqrt(mean((truth - estimate)^2))
  )

  expect_equal(
    mpe_vec(truth, estimate),
    mean(((truth - estimate) / truth) * 100)
  )

  expect_equal(
    mape_vec(truth, estimate),
    mean(abs((truth - estimate) / truth)) * 100
  )

  expect_equal(
    smape_vec(truth, estimate),
    mean(abs(estimate - truth) / ((abs(truth) + abs(estimate)) / 2)) * 100
  )
})
