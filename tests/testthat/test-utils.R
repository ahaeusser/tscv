
test_that("estimate_mode returns one numeric value", {
  x <- M4_monthly_data |>
    filter(series == first(series)) |>
    pull(value)

  mode <- estimate_mode(x)

  expect_type(mode, "double")
  expect_length(mode, 1)
  expect_true(is.finite(mode))
})


test_that("estimate_mode removes missing values by default", {
  x <- M4_monthly_data |>
    filter(series == first(series)) |>
    pull(value)

  x_with_missing <- c(x, NA_real_)

  expect_equal(
    estimate_mode(x_with_missing),
    estimate_mode(x)
  )
})


test_that("estimate_mode passes arguments to density", {
  x <- M4_monthly_data |>
    filter(series == first(series)) |>
    pull(value)

  mode <- estimate_mode(x, bw = "nrd0", n = 512)

  expect_type(mode, "double")
  expect_length(mode, 1)
  expect_true(is.finite(mode))
})


test_that("estimate_mode errors with missing values when na_rm is FALSE", {
  x <- M4_monthly_data |>
    filter(series == first(series)) |>
    pull(value)

  x_with_missing <- c(x, NA_real_)

  expect_error(
    estimate_mode(x_with_missing, na_rm = FALSE),
    "'x' contains missing values",
    fixed = TRUE
  )
})


test_that("estimate_kurtosis returns one numeric value", {
  x <- M4_monthly_data |>
    filter(series == first(series)) |>
    pull(value)

  kurtosis <- estimate_kurtosis(x)

  expect_type(kurtosis, "double")
  expect_length(kurtosis, 1)
  expect_true(is.finite(kurtosis))
  expect_gt(kurtosis, 0)
})


test_that("estimate_kurtosis matches its moment formula", {
  x <- M4_monthly_data |>
    filter(series == first(series)) |>
    pull(value)

  expected <- length(x) *
    sum((x - mean(x))^4) /
    (sum((x - mean(x))^2)^2)

  expect_equal(
    estimate_kurtosis(x),
    expected
  )
})


test_that("estimate_kurtosis removes missing values by default", {
  x <- M4_monthly_data |>
    filter(series == first(series)) |>
    pull(value)

  x_with_missing <- c(x, NA_real_)

  expect_equal(
    estimate_kurtosis(x_with_missing),
    estimate_kurtosis(x)
  )
})


test_that("estimate_kurtosis returns NA with missing values when na_rm is FALSE", {
  x <- M4_monthly_data |>
    filter(series == first(series)) |>
    pull(value)

  x_with_missing <- c(x, NA_real_)

  expect_true(is.na(estimate_kurtosis(x_with_missing, na_rm = FALSE)))
})


test_that("estimate_skewness returns one numeric value", {
  x <- M4_monthly_data |>
    filter(series == first(series)) |>
    pull(value)

  skewness <- estimate_skewness(x)

  expect_type(skewness, "double")
  expect_length(skewness, 1)
  expect_true(is.finite(skewness))
})


test_that("estimate_skewness matches its moment formula", {
  x <- M4_monthly_data |>
    filter(series == first(series)) |>
    pull(value)

  expected <- (sum((x - mean(x))^3) / length(x)) /
    (sum((x - mean(x))^2) / length(x))^(3 / 2)

  expect_equal(
    estimate_skewness(x),
    expected
  )
})


test_that("estimate_skewness removes missing values by default", {
  x <- M4_monthly_data |>
    filter(series == first(series)) |>
    pull(value)

  x_with_missing <- c(x, NA_real_)

  expect_equal(
    estimate_skewness(x_with_missing),
    estimate_skewness(x)
  )
})


test_that("estimate_skewness returns NA with missing values when na_rm is FALSE", {
  x <- M4_monthly_data |>
    filter(series == first(series)) |>
    pull(value)

  x_with_missing <- c(x, NA_real_)

  expect_true(is.na(estimate_skewness(x_with_missing, na_rm = FALSE)))
})
