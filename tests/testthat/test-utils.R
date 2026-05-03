
test_that("estimate_mode returns the KDE mode for simple numeric input", {
  x <- c(1, 1, 1, 2, 2, 3, 4)

  result <- estimate_mode(x)

  expect_type(result, "double")
  expect_length(result, 1)
  expect_true(is.finite(result))
  expect_true(result > min(x))
  expect_true(result < max(x))
})


test_that("estimate_mode removes missing values by default", {
  x <- c(1, 1, 1, 2, 2, 3, 4, NA)

  result_with_na <- estimate_mode(x)
  result_without_na <- estimate_mode(stats::na.omit(x))

  expect_equal(result_with_na, result_without_na)
})


test_that("estimate_mode errors when na_rm = FALSE and data contain NA", {
  x <- c(1, 1, 2, 3, NA)

  expect_error(
    estimate_mode(x, na_rm = FALSE),
    "'x' contains missing values",
    fixed = TRUE
  )
})


test_that("estimate_mode passes additional arguments to stats::density", {
  x <- c(1, 1, 1, 2, 2, 3, 4)

  result_default <- estimate_mode(x)
  result_adjusted <- estimate_mode(x, bw = 0.5, n = 512)

  expect_type(result_adjusted, "double")
  expect_length(result_adjusted, 1)
  expect_true(is.finite(result_adjusted))
  expect_false(is.na(result_default))
})


test_that("estimate_mode works for simulated unimodal data", {
  set.seed(123)
  x <- stats::rnorm(1000, mean = 5, sd = 1)

  result <- estimate_mode(x)

  expect_type(result, "double")
  expect_length(result, 1)
  expect_true(is.finite(result))
  expect_true(abs(result - 5) < 0.5)
})


test_that("estimate_kurtosis returns expected value for a simple vector", {
  x <- c(1, 2, 3, 4, 5)

  expected <- length(x) *
    sum((x - mean(x))^4) /
    (sum((x - mean(x))^2)^2)

  result <- estimate_kurtosis(x)

  expect_type(result, "double")
  expect_length(result, 1)
  expect_equal(result, expected)
})


test_that("estimate_kurtosis removes missing values by default", {
  x <- c(1, 2, 3, 4, 5, NA)

  result_with_na <- estimate_kurtosis(x)
  result_without_na <- estimate_kurtosis(stats::na.omit(x))

  expect_equal(result_with_na, result_without_na)
})


test_that("estimate_kurtosis returns NA when na_rm = FALSE and data contain NA", {
  x <- c(1, 2, 3, 4, 5, NA)

  result <- estimate_kurtosis(x, na_rm = FALSE)

  expect_true(is.na(result))
})


test_that("estimate_kurtosis is invariant to location and positive scale changes", {
  x <- c(1, 2, 3, 4, 5, 10)

  result_original <- estimate_kurtosis(x)
  result_transformed <- estimate_kurtosis(10 + 3 * x)

  expect_equal(result_original, result_transformed)
})


test_that("estimate_kurtosis is close to 3 for normal data", {
  set.seed(123)
  x <- stats::rnorm(5000)

  result <- estimate_kurtosis(x)

  expect_type(result, "double")
  expect_length(result, 1)
  expect_true(is.finite(result))
  expect_true(abs(result - 3) < 0.25)
})


test_that("estimate_kurtosis returns NaN for constant data", {
  x <- rep(1, 10)

  result <- estimate_kurtosis(x)

  expect_true(is.nan(result))
})


test_that("estimate_skewness returns expected value for a simple vector", {
  x <- c(1, 2, 3, 4, 10)

  expected <- (sum((x - mean(x))^3) / length(x)) /
    (sum((x - mean(x))^2) / length(x))^(3 / 2)

  result <- estimate_skewness(x)

  expect_type(result, "double")
  expect_length(result, 1)
  expect_equal(result, expected)
})


test_that("estimate_skewness removes missing values by default", {
  x <- c(1, 2, 3, 4, 10, NA)

  result_with_na <- estimate_skewness(x)
  result_without_na <- estimate_skewness(stats::na.omit(x))

  expect_equal(result_with_na, result_without_na)
})


test_that("estimate_skewness returns NA when na_rm = FALSE and data contain NA", {
  x <- c(1, 2, 3, 4, 10, NA)

  result <- estimate_skewness(x, na_rm = FALSE)

  expect_true(is.na(result))
})


test_that("estimate_skewness is invariant to location and positive scale changes", {
  x <- c(1, 2, 3, 4, 10)

  result_original <- estimate_skewness(x)
  result_transformed <- estimate_skewness(10 + 3 * x)

  expect_equal(result_original, result_transformed)
})


test_that("estimate_skewness changes sign under negative scaling", {
  x <- c(1, 2, 3, 4, 10)

  result_original <- estimate_skewness(x)
  result_reflected <- estimate_skewness(-x)

  expect_equal(result_reflected, -result_original)
})


test_that("estimate_skewness is zero for symmetric data", {
  x <- c(-2, -1, 0, 1, 2)

  result <- estimate_skewness(x)

  expect_equal(result, 0)
})


test_that("estimate_skewness is positive for right-skewed data", {
  x <- c(1, 2, 3, 4, 20)

  result <- estimate_skewness(x)

  expect_gt(result, 0)
})


test_that("estimate_skewness returns NaN for constant data", {
  x <- rep(1, 10)

  result <- estimate_skewness(x)

  expect_true(is.nan(result))
})
