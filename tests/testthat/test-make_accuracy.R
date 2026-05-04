
test_that("make_accuracy returns accuracy by horizon", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tsibble")
  skip_if_not_installed("fable")
  skip_if_not_installed("fabletools")
  skip_if_not_installed("tidyr")
  skip_if_not_installed("purrr")

  library(dplyr)
  library(tsibble)
  library(fable)
  library(fabletools)

  context <- list(
    series_id = "series",
    value_id = "value",
    index_id = "index"
  )

  main_frame <- M4_monthly_data |>
    filter(series %in% c("M23100", "M14395"))

  split_frame <- make_split(
    main_frame = main_frame,
    context = context,
    type = "first",
    value = 120,
    n_ahead = 18,
    n_skip = 17,
    n_lag = 0,
    mode = "stretch",
    exceed = FALSE
  )

  train_frame <- slice_train(
    main_frame = main_frame,
    split_frame = split_frame,
    context = context
  ) |>
    as_tsibble(
      index = index,
      key = c(series, split)
    )

  model_frame <- train_frame |>
    model(
      "SNAIVE" = SNAIVE(value ~ lag("year"))
    )

  fable_frame <- model_frame |>
    forecast(h = 18)

  future_frame <- make_future(
    fable = fable_frame,
    context = context
  )

  accuracy_horizon <- make_accuracy(
    future_frame = future_frame,
    main_frame = main_frame,
    context = context,
    dimension = "horizon"
  )

  expect_s3_class(accuracy_horizon, "tbl_df")
  expect_named(
    accuracy_horizon,
    c("series", "model", "dimension", "n", "metric", "value")
  )

  expect_equal(unique(accuracy_horizon$model), "SNAIVE")
  expect_equal(unique(accuracy_horizon$dimension), "horizon")
  expect_equal(sort(unique(accuracy_horizon$series)), c("M14395", "M23100"))
  expect_equal(sort(unique(accuracy_horizon$n)), 1:18)

  expect_equal(
    sort(unique(accuracy_horizon$metric)),
    c("MAE", "MAPE", "ME", "MPE", "MSE", "RMSE", "sMAPE")
  )

  expect_true(all(is.finite(accuracy_horizon$value)))
  expect_equal(nrow(accuracy_horizon), 2 * 1 * 18 * 7)
})


test_that("make_accuracy returns accuracy by split", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tsibble")
  skip_if_not_installed("fable")
  skip_if_not_installed("fabletools")
  skip_if_not_installed("tidyr")
  skip_if_not_installed("purrr")

  library(dplyr)
  library(tsibble)
  library(fable)
  library(fabletools)

  context <- list(
    series_id = "series",
    value_id = "value",
    index_id = "index"
  )

  main_frame <- M4_monthly_data |>
    filter(series %in% c("M23100", "M14395"))

  split_frame <- make_split(
    main_frame = main_frame,
    context = context,
    type = "first",
    value = 120,
    n_ahead = 18,
    n_skip = 17,
    n_lag = 0,
    mode = "stretch",
    exceed = FALSE
  )

  train_frame <- slice_train(
    main_frame = main_frame,
    split_frame = split_frame,
    context = context
  ) |>
    as_tsibble(
      index = index,
      key = c(series, split)
    )

  model_frame <- train_frame |>
    model(
      "SNAIVE" = SNAIVE(value ~ lag("year"))
    )

  fable_frame <- model_frame |>
    forecast(h = 18)

  future_frame <- make_future(
    fable = fable_frame,
    context = context
  )

  accuracy_split <- make_accuracy(
    future_frame = future_frame,
    main_frame = main_frame,
    context = context,
    dimension = "split"
  )

  expect_s3_class(accuracy_split, "tbl_df")
  expect_named(
    accuracy_split,
    c("series", "model", "dimension", "n", "metric", "value")
  )

  expect_equal(unique(accuracy_split$model), "SNAIVE")
  expect_equal(unique(accuracy_split$dimension), "split")
  expect_equal(sort(unique(accuracy_split$series)), c("M14395", "M23100"))

  expect_equal(
    sort(unique(accuracy_split$metric)),
    c("MAE", "MAPE", "ME", "MPE", "MSE", "RMSE", "sMAPE")
  )

  expected_rows <- future_frame |>
    distinct(series, model, split) |>
    summarise(n = n() * 7) |>
    pull(n)

  expect_equal(nrow(accuracy_split), expected_rows)
  expect_true(all(is.finite(accuracy_split$value)))
})


test_that("make_accuracy returns relative accuracy with benchmark", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tsibble")
  skip_if_not_installed("fable")
  skip_if_not_installed("fabletools")
  skip_if_not_installed("tidyr")
  skip_if_not_installed("purrr")

  library(dplyr)
  library(tsibble)
  library(fable)
  library(fabletools)

  context <- list(
    series_id = "series",
    value_id = "value",
    index_id = "index"
  )

  main_frame <- M4_monthly_data |>
    filter(series %in% c("M23100", "M14395"))

  split_frame <- make_split(
    main_frame = main_frame,
    context = context,
    type = "first",
    value = 120,
    n_ahead = 18,
    n_skip = 17,
    n_lag = 0,
    mode = "stretch",
    exceed = FALSE
  )

  train_frame <- slice_train(
    main_frame = main_frame,
    split_frame = split_frame,
    context = context
  ) |>
    as_tsibble(
      index = index,
      key = c(series, split)
    )

  model_frame <- train_frame |>
    model(
      "SNAIVE" = SNAIVE(value ~ lag("year")),
      "MEAN" = MEAN(value)
    )

  fable_frame <- model_frame |>
    forecast(h = 18)

  future_frame <- make_future(
    fable = fable_frame,
    context = context
  )

  accuracy_split <- make_accuracy(
    future_frame = future_frame,
    main_frame = main_frame,
    context = context,
    dimension = "split",
    benchmark = "SNAIVE"
  )

  expect_s3_class(accuracy_split, "tbl_df")
  expect_named(
    accuracy_split,
    c("series", "model", "dimension", "n", "metric", "value")
  )

  expect_equal(unique(accuracy_split$dimension), "split")
  expect_equal(sort(unique(accuracy_split$model)), c("MEAN", "SNAIVE"))
  expect_true("rMAE" %in% accuracy_split$metric)

  benchmark_rmae <- accuracy_split |>
    filter(model == "SNAIVE", metric == "rMAE")

  model_rmae <- accuracy_split |>
    filter(model == "MEAN", metric == "rMAE")

  expect_true(nrow(benchmark_rmae) > 0)
  expect_true(nrow(model_rmae) > 0)

  expect_equal(benchmark_rmae$value, rep(1, nrow(benchmark_rmae)))
  expect_true(all(is.finite(model_rmae$value)))
  expect_true(all(model_rmae$value > 0))
})


test_that("make_accuracy returns expected metric values", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tsibble")
  skip_if_not_installed("fable")
  skip_if_not_installed("fabletools")
  skip_if_not_installed("tidyr")
  skip_if_not_installed("purrr")

  library(dplyr)
  library(tsibble)
  library(fable)
  library(fabletools)

  context <- list(
    series_id = "series",
    value_id = "value",
    index_id = "index"
  )

  main_frame <- M4_monthly_data |>
    filter(series %in% c("M23100", "M14395"))

  split_frame <- make_split(
    main_frame = main_frame,
    context = context,
    type = "first",
    value = 120,
    n_ahead = 18,
    n_skip = 17,
    n_lag = 0,
    mode = "stretch",
    exceed = FALSE
  )

  train_frame <- slice_train(
    main_frame = main_frame,
    split_frame = split_frame,
    context = context
  ) |>
    as_tsibble(
      index = index,
      key = c(series, split)
    )

  model_frame <- train_frame |>
    model(
      "SNAIVE" = SNAIVE(value ~ lag("year"))
    )

  fable_frame <- model_frame |>
    forecast(h = 18)

  future_frame <- make_future(
    fable = fable_frame,
    context = context
  )

  accuracy_split <- make_accuracy(
    future_frame = future_frame,
    main_frame = main_frame,
    context = context,
    dimension = "split"
  )

  joined_frame <- future_frame |>
    left_join(main_frame, by = c("series", "index")) |>
    filter(series == "M14395", model == "SNAIVE", split == 1)

  expected_mae <- mae_vec(
    truth = joined_frame$value,
    estimate = joined_frame$point
  )

  expected_rmse <- rmse_vec(
    truth = joined_frame$value,
    estimate = joined_frame$point
  )

  actual_mae <- accuracy_split |>
    filter(series == "M14395", model == "SNAIVE", n == 1, metric == "MAE") |>
    pull(value)

  actual_rmse <- accuracy_split |>
    filter(series == "M14395", model == "SNAIVE", n == 1, metric == "RMSE") |>
    pull(value)

  expect_equal(actual_mae, expected_mae)
  expect_equal(actual_rmse, expected_rmse)
})
