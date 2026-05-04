
test_that("me_vec calculates mean error", {
  truth <- c(10, 20, 30)
  estimate <- c(8, 22, 25)

  expect_equal(
    me_vec(truth, estimate),
    mean(truth - estimate)
  )

  expect_equal(
    me_vec(truth, estimate),
    (2 - 2 + 5) / 3
  )
})


test_that("mae_vec calculates mean absolute error", {
  truth <- c(10, 20, 30)
  estimate <- c(8, 22, 25)

  expect_equal(
    mae_vec(truth, estimate),
    mean(abs(truth - estimate))
  )

  expect_equal(
    mae_vec(truth, estimate),
    (2 + 2 + 5) / 3
  )
})


test_that("mse_vec calculates mean squared error", {
  truth <- c(10, 20, 30)
  estimate <- c(8, 22, 25)

  expect_equal(
    mse_vec(truth, estimate),
    mean((truth - estimate)^2)
  )

  expect_equal(
    mse_vec(truth, estimate),
    (4 + 4 + 25) / 3
  )
})


test_that("rmse_vec calculates root mean squared error", {
  truth <- c(10, 20, 30)
  estimate <- c(8, 22, 25)

  expect_equal(
    rmse_vec(truth, estimate),
    sqrt(mean((truth - estimate)^2))
  )

  expect_equal(
    rmse_vec(truth, estimate),
    sqrt((4 + 4 + 25) / 3)
  )
})


test_that("mpe_vec calculates mean percentage error", {
  truth <- c(10, 20, 40)
  estimate <- c(8, 22, 30)

  expect_equal(
    mpe_vec(truth, estimate),
    mean(((truth - estimate) / truth) * 100)
  )

  expect_equal(
    mpe_vec(truth, estimate),
    mean(c(20, -10, 25))
  )
})


test_that("mape_vec calculates mean absolute percentage error", {
  truth <- c(10, 20, 40)
  estimate <- c(8, 22, 30)

  expect_equal(
    mape_vec(truth, estimate),
    mean(abs((truth - estimate) / truth)) * 100
  )

  expect_equal(
    mape_vec(truth, estimate),
    mean(c(20, 10, 25))
  )
})


test_that("smape_vec calculates symmetric mean absolute percentage error", {
  truth <- c(10, 20, 40)
  estimate <- c(8, 22, 30)

  expected <- mean(
    abs(estimate - truth) /
      ((abs(truth) + abs(estimate)) / 2)
  ) * 100

  expect_equal(
    smape_vec(truth, estimate),
    expected
  )
})


test_that("all vector accuracy metrics are zero for perfect forecasts", {
  truth <- c(10, 20, 30)
  estimate <- c(10, 20, 30)

  expect_equal(me_vec(truth, estimate), 0)
  expect_equal(mae_vec(truth, estimate), 0)
  expect_equal(mse_vec(truth, estimate), 0)
  expect_equal(rmse_vec(truth, estimate), 0)
  expect_equal(mpe_vec(truth, estimate), 0)
  expect_equal(mape_vec(truth, estimate), 0)
  expect_equal(smape_vec(truth, estimate), 0)
})


test_that("absolute and squared vector accuracy metrics are non-negative", {
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


test_that("vector accuracy metrics remove missing values by default", {
  truth <- c(10, 20, 30, NA)
  estimate <- c(8, 22, 25, 100)

  truth_complete <- c(10, 20, 30)
  estimate_complete <- c(8, 22, 25)

  expect_equal(
    me_vec(truth, estimate),
    me_vec(truth_complete, estimate_complete)
  )

  expect_equal(
    mae_vec(truth, estimate),
    mae_vec(truth_complete, estimate_complete)
  )

  expect_equal(
    mse_vec(truth, estimate),
    mse_vec(truth_complete, estimate_complete)
  )

  expect_equal(
    rmse_vec(truth, estimate),
    rmse_vec(truth_complete, estimate_complete)
  )

  expect_equal(
    mpe_vec(truth, estimate),
    mpe_vec(truth_complete, estimate_complete)
  )

  expect_equal(
    mape_vec(truth, estimate),
    mape_vec(truth_complete, estimate_complete)
  )

  expect_equal(
    smape_vec(truth, estimate),
    smape_vec(truth_complete, estimate_complete)
  )
})


test_that("vector accuracy metrics return NA with missing values when na_rm is FALSE", {
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


test_that("percentage metrics handle zero actual values consistently", {
  truth <- c(0, 10)
  estimate <- c(1, 8)

  expect_true(is.infinite(mpe_vec(truth, estimate)))
  expect_true(is.infinite(mape_vec(truth, estimate)))
  expect_true(is.finite(smape_vec(truth, estimate)))
})


test_that("percentage metrics return NaN when actual and forecast are both zero", {
  truth <- c(0)
  estimate <- c(0)

  expect_true(is.nan(mpe_vec(truth, estimate)))
  expect_true(is.nan(mape_vec(truth, estimate)))
  expect_true(is.nan(smape_vec(truth, estimate)))
})
