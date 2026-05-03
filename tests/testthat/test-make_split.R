
make_context <- function() {
  list(
    series_id = "bidding_zone",
    value_id = "value",
    index_id = "time"
  )
}


make_price_frame <- function(n_per_series = 120) {
  elec_price |>
    dplyr::filter(bidding_zone %in% c("DE", "FR")) |>
    dplyr::group_by(bidding_zone) |>
    dplyr::slice_head(n = n_per_series) |>
    dplyr::ungroup()
}


get_split <- function(split_frame, series, split_id) {
  split_frame |>
    dplyr::filter(
      bidding_zone == series,
      split == split_id
    )
}


expect_split_frame <- function(x, n_series, n_splits) {
  expect_s3_class(x, "tbl_df")
  expect_named(x, c("bidding_zone", "split", "train", "test"))

  expect_equal(nrow(x), n_series * n_splits)
  expect_equal(sort(unique(x$bidding_zone)), c("DE", "FR"))
  expect_equal(sort(unique(x$split)), seq_len(n_splits))

  expect_true(is.list(x$train))
  expect_true(is.list(x$test))
}


test_that("make_split creates fixed-window splits with type = first", {
  skip_if_not_installed("dplyr")

  context <- make_context()
  main_frame <- make_price_frame(n_per_series = 120)

  fixed_split <- make_split(
    main_frame = main_frame,
    context = context,
    type = "first",
    value = 48,
    n_ahead = 24,
    n_skip = 23,
    n_lag = 0,
    mode = "slide",
    exceed = FALSE
  )

  expect_split_frame(fixed_split, n_series = 2, n_splits = 3)

  de_1 <- get_split(fixed_split, "DE", 1)
  de_2 <- get_split(fixed_split, "DE", 2)
  de_3 <- get_split(fixed_split, "DE", 3)

  expect_equal(de_1$train[[1]], 1:48)
  expect_equal(de_1$test[[1]], 49:72)

  expect_equal(de_2$train[[1]], 25:72)
  expect_equal(de_2$test[[1]], 73:96)

  expect_equal(de_3$train[[1]], 49:96)
  expect_equal(de_3$test[[1]], 97:120)
})


test_that("make_split creates expanding-window splits with type = first", {
  skip_if_not_installed("dplyr")

  context <- make_context()
  main_frame <- make_price_frame(n_per_series = 120)

  expanding_split <- make_split(
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

  expect_split_frame(expanding_split, n_series = 2, n_splits = 3)

  de_1 <- get_split(expanding_split, "DE", 1)
  de_2 <- get_split(expanding_split, "DE", 2)
  de_3 <- get_split(expanding_split, "DE", 3)

  expect_equal(de_1$train[[1]], 1:48)
  expect_equal(de_1$test[[1]], 49:72)

  expect_equal(de_2$train[[1]], 1:72)
  expect_equal(de_2$test[[1]], 73:96)

  expect_equal(de_3$train[[1]], 1:96)
  expect_equal(de_3$test[[1]], 97:120)
})


test_that("make_split supports type = last", {
  skip_if_not_installed("dplyr")

  context <- make_context()
  main_frame <- make_price_frame(n_per_series = 120)

  split_frame <- make_split(
    main_frame = main_frame,
    context = context,
    type = "last",
    value = 24,
    n_ahead = 12,
    n_skip = 11,
    n_lag = 0,
    mode = "slide",
    exceed = FALSE
  )

  expect_split_frame(split_frame, n_series = 2, n_splits = 2)

  de_1 <- get_split(split_frame, "DE", 1)
  de_2 <- get_split(split_frame, "DE", 2)

  # n_init = n_total - value - 1 = 120 - 24 - 1 = 95
  expect_equal(de_1$train[[1]], 1:95)
  expect_equal(de_1$test[[1]], 96:107)

  expect_equal(de_2$train[[1]], 13:107)
  expect_equal(de_2$test[[1]], 108:119)
})


test_that("make_split supports type = prob", {
  skip_if_not_installed("dplyr")

  context <- make_context()
  main_frame <- make_price_frame(n_per_series = 120)

  split_frame <- make_split(
    main_frame = main_frame,
    context = context,
    type = "prob",
    value = 0.5,
    n_ahead = 20,
    n_skip = 19,
    n_lag = 0,
    mode = "slide",
    exceed = FALSE
  )

  expect_split_frame(split_frame, n_series = 2, n_splits = 3)

  de_1 <- get_split(split_frame, "DE", 1)
  de_2 <- get_split(split_frame, "DE", 2)
  de_3 <- get_split(split_frame, "DE", 3)

  # n_init = floor(0.5 * 120) = 60
  expect_equal(de_1$train[[1]], 1:60)
  expect_equal(de_1$test[[1]], 61:80)

  expect_equal(de_2$train[[1]], 21:80)
  expect_equal(de_2$test[[1]], 81:100)

  expect_equal(de_3$train[[1]], 41:100)
  expect_equal(de_3$test[[1]], 101:120)
})


test_that("make_split includes lagged observations in test windows", {
  skip_if_not_installed("dplyr")

  context <- make_context()
  main_frame <- make_price_frame(n_per_series = 120)

  split_frame <- make_split(
    main_frame = main_frame,
    context = context,
    type = "first",
    value = 48,
    n_ahead = 24,
    n_skip = 23,
    n_lag = 2,
    mode = "slide",
    exceed = FALSE
  )

  expect_split_frame(split_frame, n_series = 2, n_splits = 3)

  de_1 <- get_split(split_frame, "DE", 1)
  de_2 <- get_split(split_frame, "DE", 2)

  expect_equal(de_1$train[[1]], 1:48)
  expect_equal(de_1$test[[1]], 47:72)

  expect_equal(de_2$train[[1]], 25:72)
  expect_equal(de_2$test[[1]], 71:96)
})


test_that("make_split creates out-of-sample splits when exceed = TRUE", {
  skip_if_not_installed("dplyr")

  context <- make_context()
  main_frame <- make_price_frame(n_per_series = 120)

  split_frame <- make_split(
    main_frame = main_frame,
    context = context,
    type = "first",
    value = 96,
    n_ahead = 24,
    n_skip = 23,
    n_lag = 0,
    mode = "slide",
    exceed = TRUE
  )

  expect_split_frame(split_frame, n_series = 2, n_splits = 2)

  de_1 <- get_split(split_frame, "DE", 1)
  de_2 <- get_split(split_frame, "DE", 2)

  expect_equal(de_1$train[[1]], 1:96)
  expect_equal(de_1$test[[1]], 97:120)

  expect_equal(de_2$train[[1]], 25:120)
  expect_equal(de_2$test[[1]], 121:144)
})


test_that("make_split does not create out-of-sample splits when exceed = FALSE", {
  skip_if_not_installed("dplyr")

  context <- make_context()
  main_frame <- make_price_frame(n_per_series = 120)

  split_frame <- make_split(
    main_frame = main_frame,
    context = context,
    type = "first",
    value = 96,
    n_ahead = 24,
    n_skip = 23,
    n_lag = 0,
    mode = "slide",
    exceed = FALSE
  )

  expect_split_frame(split_frame, n_series = 2, n_splits = 1)

  de_1 <- get_split(split_frame, "DE", 1)

  expect_equal(de_1$train[[1]], 1:96)
  expect_equal(de_1$test[[1]], 97:120)
})


test_that("make_split keeps series-specific split plans separate", {
  skip_if_not_installed("dplyr")

  context <- make_context()
  main_frame <- make_price_frame(n_per_series = 120)

  split_frame <- make_split(
    main_frame = main_frame,
    context = context,
    type = "first",
    value = 48,
    n_ahead = 24,
    n_skip = 23,
    n_lag = 0,
    mode = "slide",
    exceed = FALSE
  )

  de_split <- get_split(split_frame, "DE", 1)
  fr_split <- get_split(split_frame, "FR", 1)

  expect_equal(de_split$train[[1]], fr_split$train[[1]])
  expect_equal(de_split$test[[1]], fr_split$test[[1]])
  expect_equal(de_split$bidding_zone, "DE")
  expect_equal(fr_split$bidding_zone, "FR")
})


test_that("make_split errors when the initial window and horizon exceed the sample size", {
  skip_if_not_installed("dplyr")

  context <- make_context()
  main_frame <- make_price_frame(n_per_series = 120)

  expect_error(
    make_split(
      main_frame = main_frame,
      context = context,
      type = "first",
      value = 110,
      n_ahead = 20,
      n_skip = 0,
      n_lag = 0,
      mode = "slide",
      exceed = FALSE
    ),
    "There should be at least 130 observations in `data`",
    fixed = TRUE
  )
})


test_that("make_split errors when n_lag is not a whole number", {
  skip_if_not_installed("dplyr")

  context <- make_context()
  main_frame <- make_price_frame(n_per_series = 120)

  expect_error(
    make_split(
      main_frame = main_frame,
      context = context,
      type = "first",
      value = 48,
      n_ahead = 24,
      n_skip = 23,
      n_lag = 1.5,
      mode = "slide",
      exceed = FALSE
    ),
    "`n_lag` must be a whole number.",
    fixed = TRUE
  )
})


test_that("make_split errors when n_lag is larger than the training window", {
  skip_if_not_installed("dplyr")

  context <- make_context()
  main_frame <- make_price_frame(n_per_series = 120)

  expect_error(
    make_split(
      main_frame = main_frame,
      context = context,
      type = "first",
      value = 48,
      n_ahead = 24,
      n_skip = 23,
      n_lag = 49,
      mode = "slide",
      exceed = FALSE
    ),
    "`n_lag` must be less than or equal to the number of training observations.",
    fixed = TRUE
  )
})
