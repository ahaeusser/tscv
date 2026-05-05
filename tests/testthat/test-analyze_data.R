
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


test_that("acf_vec returns autocorrelations without lag zero", {
  library(dplyr)

  x <- M4_monthly_data |>
    filter(series == first(series)) |>
    pull(value)

  result <- acf_vec(
    x = x,
    lag_max = 12
  )

  expected <- as.numeric(acf(
    x = x,
    lag.max = 12,
    plot = FALSE
  )$acf)
  expected <- tail(expected, -1)

  expect_type(result, "double")
  expect_length(result, 12)
  expect_equal(result, expected)
})


test_that("pacf_vec returns partial autocorrelations", {
  library(dplyr)

  x <- M4_monthly_data |>
    filter(series == first(series)) |>
    pull(value)

  result <- pacf_vec(
    x = x,
    lag_max = 12
  )

  expected <- as.numeric(pacf(
    x = x,
    lag.max = 12,
    plot = FALSE
  )$acf)

  expect_type(result, "double")
  expect_length(result, 12)
  expect_equal(result, expected)
})


test_that("estimate_acf returns autocorrelations by series", {
  library(dplyr)

  context <- list(
    series_id = "series",
    value_id = "value",
    index_id = "index"
  )

  data <- M4_monthly_data |>
    filter(series %in% c("M23100", "M14395"))

  result <- estimate_acf(
    .data = data,
    context = context,
    lag_max = 12,
    level = 0.9
  )

  expect_s3_class(result, "tbl_df")
  expect_named(
    result,
    c("series", "type", "lag", "value", "bound", "sign")
  )

  expect_equal(sort(unique(result$series)), c("M14395", "M23100"))
  expect_equal(unique(result$type), "ACF")
  expect_equal(sort(unique(result$lag)), 1:12)
  expect_equal(nrow(result), 2 * 12)

  expect_type(result$value, "double")
  expect_type(result$bound, "double")
  expect_type(result$sign, "logical")

  expect_true(all(is.finite(result$value)))
  expect_true(all(is.finite(result$bound)))
  expect_true(all(result$bound > 0))

  expected_bound <- abs(qnorm((1 - 0.9) / 2) / sqrt(
    nrow(filter(data, series == "M23100"))
  ))

  result_bound <- result |>
    filter(series == "M23100") |>
    pull(bound) |>
    unique()

  expect_equal(result_bound, expected_bound)
})


test_that("estimate_pacf returns partial autocorrelations by series", {
  library(dplyr)

  context <- list(
    series_id = "series",
    value_id = "value",
    index_id = "index"
  )

  data <- M4_monthly_data |>
    filter(series %in% c("M23100", "M14395"))

  result <- estimate_pacf(
    .data = data,
    context = context,
    lag_max = 12,
    level = 0.9
  )

  expect_s3_class(result, "tbl_df")
  expect_named(
    result,
    c("series", "type", "lag", "value", "bound", "sign")
  )

  expect_equal(sort(unique(result$series)), c("M14395", "M23100"))
  expect_equal(unique(result$type), "PACF")
  expect_equal(sort(unique(result$lag)), 1:12)
  expect_equal(nrow(result), 2 * 12)

  expect_type(result$value, "double")
  expect_type(result$bound, "double")
  expect_type(result$sign, "logical")

  expect_true(all(is.finite(result$value)))
  expect_true(all(is.finite(result$bound)))
  expect_true(all(result$bound > 0))
})


test_that("summarise_data returns data-quality statistics by series", {
  library(dplyr)

  context <- list(
    series_id = "series",
    value_id = "value",
    index_id = "index"
  )

  data <- M4_monthly_data |>
    filter(series %in% c("M23100", "M14395"))

  result <- summarise_data(
    .data = data,
    context = context
  )

  expect_s3_class(result, "tbl_df")
  expect_named(
    result,
    c(
      "series",
      "start",
      "end",
      "n_obs",
      "n_missing",
      "pct_missing",
      "n_zeros",
      "pct_zeros"
    )
  )

  expect_equal(sort(result$series), c("M14395", "M23100"))
  expect_equal(nrow(result), 2)

  m23100 <- data |>
    filter(series == "M23100")

  result_m23100 <- result |>
    filter(series == "M23100")

  expect_equal(result_m23100$start, first(m23100$index))
  expect_equal(result_m23100$end, last(m23100$index))
  expect_equal(result_m23100$n_obs, nrow(m23100))
  expect_equal(result_m23100$n_missing, sum(is.na(m23100$value)))
  expect_equal(result_m23100$n_zeros, sum(m23100$value == 0, na.rm = TRUE))
  expect_equal(
    result_m23100$pct_missing,
    round((result_m23100$n_missing / result_m23100$n_obs) * 100, 2)
  )
  expect_equal(
    result_m23100$pct_zeros,
    round((result_m23100$n_zeros / result_m23100$n_obs) * 100, 2)
  )
})


test_that("summarise_stats returns descriptive statistics by series", {
  library(dplyr)

  context <- list(
    series_id = "series",
    value_id = "value",
    index_id = "index"
  )

  data <- M4_monthly_data |>
    filter(series %in% c("M23100", "M14395"))

  result <- summarise_stats(
    .data = data,
    context = context
  )

  expect_s3_class(result, "tbl_df")
  expect_named(
    result,
    c(
      "series",
      "mean",
      "median",
      "mode",
      "sd",
      "p0",
      "p25",
      "p75",
      "p100",
      "skewness",
      "kurtosis"
    )
  )

  expect_equal(sort(result$series), c("M14395", "M23100"))
  expect_equal(nrow(result), 2)

  m23100 <- data |>
    filter(series == "M23100") |>
    pull(value)

  result_m23100 <- result |>
    filter(series == "M23100")

  expect_equal(result_m23100$mean, mean(m23100, na.rm = TRUE))
  expect_equal(result_m23100$median, median(m23100, na.rm = TRUE))
  expect_equal(result_m23100$sd, sd(m23100, na.rm = TRUE))

  # expect_equal(result_m23100$p0, as.numeric(quantile(m23100, probs = 0, na.rm = TRUE)))
  # expect_equal(result_m23100$p25, as.numeric(quantile(m23100, probs = 0.25, na.rm = TRUE)))
  # expect_equal(result_m23100$p75, as.numeric(quantile(m23100, probs = 0.75, na.rm = TRUE)))
  # expect_equal(result_m23100$p100, as.numeric(quantile(m23100, probs = 1, na.rm = TRUE)))

  expect_equal(
    unname(result_m23100$p0),
    as.numeric(quantile(m23100, probs = 0, na.rm = TRUE))
  )

  expect_equal(
    unname(result_m23100$p25),
    as.numeric(quantile(m23100, probs = 0.25, na.rm = TRUE))
  )

  expect_equal(
    unname(result_m23100$p75),
    as.numeric(quantile(m23100, probs = 0.75, na.rm = TRUE))
  )

  expect_equal(
    unname(result_m23100$p100),
    as.numeric(quantile(m23100, probs = 1, na.rm = TRUE))
  )

  expect_true(is.finite(result_m23100$mode))
  expect_true(is.finite(result_m23100$skewness))
  expect_true(is.finite(result_m23100$kurtosis))
})


test_that("summarise_split returns train and test ranges by split", {
  library(dplyr)
  library(tsibble)

  context <- list(
    series_id = "bidding_zone",
    value_id = "value",
    index_id = "time"
  )

  main_frame <- elec_price |>
    filter(bidding_zone == "DE") |>
    slice_head(n = 120)

  split_frame <- make_split(
    main_frame = main_frame,
    context = context,
    type = "first",
    value = 48,
    n_ahead = 24,
    n_skip = 23,
    n_lag = 0,
    mode = "stretch",
    exceed = FALSE
  )

  train_frame <- slice_train(
    main_frame = main_frame,
    split_frame = split_frame,
    context = context
  ) |>
    mutate(sample = "train")

  test_frame <- slice_test(
    main_frame = main_frame,
    split_frame = split_frame,
    context = context
  ) |>
    mutate(sample = "test")

  split_data <- bind_rows(train_frame, test_frame) |>
    arrange(bidding_zone, split, sample, time) |>
    group_by(bidding_zone, split, sample) |>
    mutate(id = row_number()) |>
    ungroup() |>
    as_tsibble(
      index = time,
      key = c(bidding_zone, split, sample)
    )

  result <- summarise_split(split_data)

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("split", "time_test", "time_train", "index_test", "index_train") %in% names(result)))

  expect_equal(result$split, 1:3)
  expect_equal(nrow(result), 3)

  expect_equal(result$index_train[1], "[01, 48]")
  expect_equal(result$index_test[1], "[01, 24]")

  expect_match(result$time_train[1], "^\\[")
  expect_match(result$time_train[1], "\\]$")
  expect_match(result$time_test[1], "^\\[")
  expect_match(result$time_test[1], "\\]$")
})
