
test_that("make_future() converts a tsibble-like forecast object to a future_frame", {
  skip_if_not_installed("tsibble")

  fable <- tibble::tibble(
    date = as.Date(c(
      "2024-01-01", "2024-01-02", "2024-01-03",
      "2024-01-01", "2024-01-02", "2024-01-03"
    )),
    id = c("a", "a", "a", "b", "b", "b"),
    split = c(1L, 1L, 1L, 1L, 1L, 1L),
    .model = c("arima", "arima", "arima", "arima", "arima", "arima"),
    y = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
    .mean = c(10, 11, 12, 20, 21, 22)
  ) |>
    tsibble::as_tsibble(
      key = c(id, split, .model),
      index = date
    )

  context <- list(
    series_id = "id",
    value_id = "y",
    index_id = "date"
  )

  result <- make_future(
    fable = fable,
    context = context
  )

  expected <- tibble::tibble(
    date = as.Date(c(
      "2024-01-01", "2024-01-02", "2024-01-03",
      "2024-01-01", "2024-01-02", "2024-01-03"
    )),
    id = c("a", "a", "a", "b", "b", "b"),
    model = c("arima", "arima", "arima", "arima", "arima", "arima"),
    split = c(1L, 1L, 1L, 1L, 1L, 1L),
    horizon = c(1L, 2L, 3L, 1L, 2L, 3L),
    point = c(10, 11, 12, 20, 21, 22)
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(result, expected)
})


test_that("make_future() calculates horizon separately by series, split, and model", {
  skip_if_not_installed("tsibble")

  fable <- tibble::tibble(
    date = as.Date(c(
      "2024-01-01", "2024-01-02",
      "2024-01-01", "2024-01-02",
      "2024-01-03", "2024-01-04"
    )),
    id = c("a", "a", "a", "a", "a", "a"),
    split = c(1L, 1L, 1L, 1L, 2L, 2L),
    .model = c("arima", "arima", "ets", "ets", "arima", "arima"),
    y = rep(NA_real_, 6),
    .mean = c(10, 11, 12, 13, 14, 15)
  ) |>
    tsibble::as_tsibble(
      key = c(id, split, .model),
      index = date
    )

  context <- list(
    series_id = "id",
    value_id = "y",
    index_id = "date"
  )

  result <- make_future(
    fable = fable,
    context = context
  )

  expected <- tibble::tibble(
    date = as.Date(c(
      "2024-01-01", "2024-01-02",
      "2024-01-01", "2024-01-02",
      "2024-01-03", "2024-01-04"
    )),
    id = c("a", "a", "a", "a", "a", "a"),
    model = c("arima", "arima", "ets", "ets", "arima", "arima"),
    split = c(1L, 1L, 1L, 1L, 2L, 2L),
    horizon = c(1L, 2L, 1L, 2L, 1L, 2L),
    point = c(10, 11, 12, 13, 14, 15)
  )

  expect_equal(result, expected)
})


test_that("make_future() removes original response and renames forecast columns", {
  skip_if_not_installed("tsibble")

  fable <- tibble::tibble(
    date = as.Date(c("2024-01-01", "2024-01-02")),
    id = c("a", "a"),
    split = c(1L, 1L),
    .model = c("arima", "arima"),
    y = c(NA_real_, NA_real_),
    .mean = c(10, 11)
  ) |>
    tsibble::as_tsibble(
      key = c(id, split, .model),
      index = date
    )

  context <- list(
    series_id = "id",
    value_id = "y",
    index_id = "date"
  )

  result <- make_future(
    fable = fable,
    context = context
  )

  expect_false("y" %in% names(result))
  expect_false(".mean" %in% names(result))
  expect_false(".model" %in% names(result))

  expect_true("point" %in% names(result))
  expect_true("model" %in% names(result))
})


test_that("make_future() preserves additional forecast columns", {
  skip_if_not_installed("tsibble")

  fable <- tibble::tibble(
    date = as.Date(c("2024-01-01", "2024-01-02")),
    id = c("a", "a"),
    split = c(1L, 1L),
    .model = c("arima", "arima"),
    y = c(NA_real_, NA_real_),
    .mean = c(10, 11),
    lower_80 = c(8, 9),
    upper_80 = c(12, 13)
  ) |>
    tsibble::as_tsibble(
      key = c(id, split, .model),
      index = date
    )

  context <- list(
    series_id = "id",
    value_id = "y",
    index_id = "date"
  )

  result <- make_future(
    fable = fable,
    context = context
  )

  expect_named(
    result,
    c("date", "id", "model", "split", "horizon", "point", "lower_80", "upper_80")
  )

  expect_equal(result$lower_80, c(8, 9))
  expect_equal(result$upper_80, c(12, 13))
})

