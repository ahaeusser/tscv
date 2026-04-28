
test_that("initialize_split() calculates initial training size by first observations", {
  main_frame <- tibble::tibble(
    id = c(rep("a", 10), rep("b", 8)),
    time = c(1:10, 1:8),
    value = rnorm(18)
  )

  context <- list(
    series_id = "id",
    index_id = "time",
    value_id = "value"
  )

  result <- initialize_split(
    main_frame = main_frame,
    context = context,
    type = "first",
    value = 5
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_equal(result$n_total, c(10, 8))
  expect_equal(result$n_init, c(5, 5))
})


test_that("initialize_split() calculates initial training size by last test observations", {
  main_frame <- tibble::tibble(
    id = c(rep("a", 10), rep("b", 8)),
    time = c(1:10, 1:8),
    value = rnorm(18)
  )

  context <- list(series_id = "id")

  result <- initialize_split(
    main_frame = main_frame,
    context = context,
    type = "last",
    value = 2
  )

  expect_equal(result$n_total, c(10, 8))
  expect_equal(result$n_init, c(7, 5))
})


test_that("initialize_split() calculates initial training size by probability", {
  main_frame <- tibble::tibble(
    id = c(rep("a", 10), rep("b", 9)),
    time = c(1:10, 1:9),
    value = rnorm(19)
  )

  context <- list(series_id = "id")

  result <- initialize_split(
    main_frame = main_frame,
    context = context,
    type = "prob",
    value = 0.6
  )

  expect_equal(result$n_total, c(10, 9))
  expect_equal(result$n_init, c(6, 5))
})


test_that("split_index() creates sliding-window train and test indices", {
  result <- split_index(
    n_total = 10,
    n_init = 4,
    n_ahead = 2,
    n_skip = 0,
    n_lag = 0,
    mode = "slide",
    exceed = FALSE
  )

  expect_type(result, "list")
  expect_named(result, c("train", "test"))

  expect_equal(length(result$train), 5)
  expect_equal(length(result$test), 5)

  expect_equal(result$train[[1]], 1:4)
  expect_equal(result$test[[1]], 5:6)

  expect_equal(result$train[[2]], 2:5)
  expect_equal(result$test[[2]], 6:7)

  expect_equal(result$train[[5]], 5:8)
  expect_equal(result$test[[5]], 9:10)
})


test_that("split_index() creates stretching-window train and test indices", {
  result <- split_index(
    n_total = 10,
    n_init = 4,
    n_ahead = 2,
    n_skip = 0,
    n_lag = 0,
    mode = "stretch",
    exceed = FALSE
  )

  expect_equal(length(result$train), 5)

  expect_equal(result$train[[1]], 1:4)
  expect_equal(result$test[[1]], 5:6)

  expect_equal(result$train[[2]], 1:5)
  expect_equal(result$test[[2]], 6:7)

  expect_equal(result$train[[5]], 1:8)
  expect_equal(result$test[[5]], 9:10)
})


test_that("split_index() respects n_skip", {
  result <- split_index(
    n_total = 12,
    n_init = 4,
    n_ahead = 2,
    n_skip = 1,
    n_lag = 0,
    mode = "slide",
    exceed = FALSE
  )

  expect_equal(length(result$train), 4)

  expect_equal(result$train[[1]], 1:4)
  expect_equal(result$test[[1]], 5:6)

  expect_equal(result$train[[2]], 3:6)
  expect_equal(result$test[[2]], 7:8)

  expect_equal(result$train[[4]], 7:10)
  expect_equal(result$test[[4]], 11:12)
})


test_that("split_index() includes lagged observations in test indices", {
  result <- split_index(
    n_total = 10,
    n_init = 4,
    n_ahead = 2,
    n_skip = 0,
    n_lag = 1,
    mode = "slide",
    exceed = FALSE
  )

  expect_equal(result$train[[1]], 1:4)
  expect_equal(result$test[[1]], 4:6)

  expect_equal(result$train[[2]], 2:5)
  expect_equal(result$test[[2]], 5:7)
})


test_that("split_index() can create splits exceeding observed sample size", {
  result <- split_index(
    n_total = 10,
    n_init = 4,
    n_ahead = 2,
    n_skip = 0,
    n_lag = 0,
    mode = "slide",
    exceed = TRUE
  )

  expect_equal(length(result$train), 7)

  expect_equal(result$train[[7]], 7:10)
  expect_equal(result$test[[7]], 11:12)
})


test_that("split_index() errors when not enough observations are available", {
  expect_error(
    split_index(
      n_total = 5,
      n_init = 4,
      n_ahead = 2,
      exceed = FALSE
    ),
    "There should be at least 6 observations in `data`"
  )
})


test_that("split_index() errors when n_lag is not a whole number", {
  expect_error(
    split_index(
      n_total = 10,
      n_init = 4,
      n_ahead = 2,
      n_lag = 1.5
    ),
    "`n_lag` must be a whole number."
  )
})


test_that("split_index() errors when n_lag exceeds n_init", {
  expect_error(
    split_index(
      n_total = 10,
      n_init = 4,
      n_ahead = 2,
      n_lag = 5
    ),
    "`n_lag` must be less than or equal to the number of training observations."
  )
})


test_that("expand_split() expands nested train and test indices", {
  split_frame <- tibble::tibble(
    id = "a",
    n_total = 6,
    n_init = 3,
    n_ahead = 2,
    n_splits = 2,
    train = list(list(1:3, 2:4)),
    test = list(list(4:5, 5:6))
  )

  context <- list(series_id = "id")

  result <- expand_split(
    split_frame = split_frame,
    context = context
  )

  expected <- tibble::tibble(
    id = c("a", "a"),
    split = c(1L, 2L),
    train = list(1:3, 2:4),
    test = list(4:5, 5:6)
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(result, expected)
})


test_that("make_split() creates expanded split frame for one series", {
  main_frame <- tibble::tibble(
    id = rep("a", 6),
    time = 1:6,
    value = 1:6
  )

  context <- list(
    series_id = "id",
    index_id = "time",
    value_id = "value"
  )

  result <- make_split(
    main_frame = main_frame,
    context = context,
    type = "first",
    value = 3,
    n_ahead = 2,
    n_skip = 0,
    n_lag = 0,
    mode = "slide",
    exceed = FALSE
  )

  expected <- tibble::tibble(
    id = c("a", "a"),
    split = c(1L, 2L),
    train = list(1:3, 2:4),
    test = list(4:5, 5:6)
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(result, expected)
})


test_that("make_split() creates expanded split frame for multiple series", {
  main_frame <- tibble::tibble(
    id = c(rep("a", 6), rep("b", 5)),
    time = c(1:6, 1:5),
    value = seq_len(11)
  )

  context <- list(
    series_id = "id",
    index_id = "time",
    value_id = "value"
  )

  result <- make_split(
    main_frame = main_frame,
    context = context,
    type = "first",
    value = 3,
    n_ahead = 2,
    n_skip = 0,
    n_lag = 0,
    mode = "slide",
    exceed = FALSE
  )

  expected <- tibble::tibble(
    id = c("a", "a", "b"),
    split = c(1L, 2L, 1L),
    train = list(1:3, 2:4, 1:3),
    test = list(4:5, 5:6, 4:5)
  )

  expect_equal(result, expected)
})
