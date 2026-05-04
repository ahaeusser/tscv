
test_that("split_index returns fixed-window indices", {
  index <- split_index(
    n_total = 120,
    n_init = 48,
    n_ahead = 24,
    n_skip = 23,
    n_lag = 0,
    mode = "slide",
    exceed = FALSE
  )

  expect_type(index, "list")
  expect_named(index, c("train", "test"))
  expect_equal(length(index$train), 3)
  expect_equal(length(index$test), 3)

  expect_equal(index$train[[1]], 1:48)
  expect_equal(index$test[[1]], 49:72)

  expect_equal(index$train[[2]], 25:72)
  expect_equal(index$test[[2]], 73:96)

  expect_equal(index$train[[3]], 49:96)
  expect_equal(index$test[[3]], 97:120)
})


test_that("split_index returns expanding-window indices", {
  index <- split_index(
    n_total = 120,
    n_init = 48,
    n_ahead = 24,
    n_skip = 23,
    n_lag = 0,
    mode = "stretch",
    exceed = FALSE
  )

  expect_type(index, "list")
  expect_named(index, c("train", "test"))
  expect_equal(length(index$train), 3)
  expect_equal(length(index$test), 3)

  expect_equal(index$train[[1]], 1:48)
  expect_equal(index$test[[1]], 49:72)

  expect_equal(index$train[[2]], 1:72)
  expect_equal(index$test[[2]], 73:96)

  expect_equal(index$train[[3]], 1:96)
  expect_equal(index$test[[3]], 97:120)
})


test_that("split_index includes lagged observations in test indices", {
  index <- split_index(
    n_total = 120,
    n_init = 48,
    n_ahead = 24,
    n_skip = 23,
    n_lag = 2,
    mode = "slide",
    exceed = FALSE
  )

  expect_equal(length(index$train), 3)
  expect_equal(length(index$test), 3)

  expect_equal(index$train[[1]], 1:48)
  expect_equal(index$test[[1]], 47:72)

  expect_equal(index$train[[2]], 25:72)
  expect_equal(index$test[[2]], 71:96)
})


test_that("split_index allows test indices to exceed the sample size", {
  index <- split_index(
    n_total = 120,
    n_init = 96,
    n_ahead = 24,
    n_skip = 23,
    n_lag = 0,
    mode = "slide",
    exceed = TRUE
  )

  expect_equal(length(index$train), 2)
  expect_equal(length(index$test), 2)

  expect_equal(index$train[[1]], 1:96)
  expect_equal(index$test[[1]], 97:120)

  expect_equal(index$train[[2]], 25:120)
  expect_equal(index$test[[2]], 121:144)
})


test_that("split_index throws errors for invalid arguments", {
  expect_error(
    split_index(
      n_total = 120,
      n_init = 110,
      n_ahead = 20,
      n_skip = 0,
      n_lag = 0,
      mode = "slide",
      exceed = FALSE
    ),
    "There should be at least 130 observations in `data`",
    fixed = TRUE
  )

  expect_error(
    split_index(
      n_total = 120,
      n_init = 48,
      n_ahead = 24,
      n_skip = 23,
      n_lag = 1.5,
      mode = "slide",
      exceed = FALSE
    ),
    "`n_lag` must be a whole number.",
    fixed = TRUE
  )

  expect_error(
    split_index(
      n_total = 120,
      n_init = 48,
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


test_that("make_split returns fixed-window splits", {
  skip_if_not_installed("dplyr")

  library(dplyr)

  context <- list(
    series_id = "bidding_zone",
    value_id = "value",
    index_id = "time"
  )

  main_frame <- elec_price |>
    filter(bidding_zone %in% c("DE", "FR")) |>
    group_by(bidding_zone) |>
    slice_head(n = 120) |>
    ungroup()

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

  expect_s3_class(split_frame, "tbl_df")
  expect_named(split_frame, c("bidding_zone", "split", "train", "test"))
  expect_equal(sort(unique(split_frame$bidding_zone)), c("DE", "FR"))
  expect_equal(sort(unique(split_frame$split)), 1:3)
  expect_equal(nrow(split_frame), 2 * 3)

  de_split_1 <- split_frame |>
    filter(bidding_zone == "DE", split == 1)

  de_split_2 <- split_frame |>
    filter(bidding_zone == "DE", split == 2)

  de_split_3 <- split_frame |>
    filter(bidding_zone == "DE", split == 3)

  expect_equal(de_split_1$train[[1]], 1:48)
  expect_equal(de_split_1$test[[1]], 49:72)

  expect_equal(de_split_2$train[[1]], 25:72)
  expect_equal(de_split_2$test[[1]], 73:96)

  expect_equal(de_split_3$train[[1]], 49:96)
  expect_equal(de_split_3$test[[1]], 97:120)
})


test_that("make_split returns expanding-window splits", {
  skip_if_not_installed("dplyr")

  library(dplyr)

  context <- list(
    series_id = "bidding_zone",
    value_id = "value",
    index_id = "time"
  )

  main_frame <- elec_price |>
    filter(bidding_zone %in% c("DE", "FR")) |>
    group_by(bidding_zone) |>
    slice_head(n = 120) |>
    ungroup()

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

  expect_s3_class(split_frame, "tbl_df")
  expect_named(split_frame, c("bidding_zone", "split", "train", "test"))
  expect_equal(sort(unique(split_frame$bidding_zone)), c("DE", "FR"))
  expect_equal(sort(unique(split_frame$split)), 1:3)
  expect_equal(nrow(split_frame), 2 * 3)

  de_split_1 <- split_frame |>
    filter(bidding_zone == "DE", split == 1)

  de_split_2 <- split_frame |>
    filter(bidding_zone == "DE", split == 2)

  de_split_3 <- split_frame |>
    filter(bidding_zone == "DE", split == 3)

  expect_equal(de_split_1$train[[1]], 1:48)
  expect_equal(de_split_1$test[[1]], 49:72)

  expect_equal(de_split_2$train[[1]], 1:72)
  expect_equal(de_split_2$test[[1]], 73:96)

  expect_equal(de_split_3$train[[1]], 1:96)
  expect_equal(de_split_3$test[[1]], 97:120)
})


test_that("make_split supports type first", {
  skip_if_not_installed("dplyr")

  library(dplyr)

  context <- list(
    series_id = "bidding_zone",
    value_id = "value",
    index_id = "time"
  )

  main_frame <- elec_price |>
    filter(bidding_zone %in% c("DE", "FR")) |>
    group_by(bidding_zone) |>
    slice_head(n = 120) |>
    ungroup()

  split_frame <- make_split(
    main_frame = main_frame,
    context = context,
    type = "first",
    value = 60,
    n_ahead = 20,
    n_skip = 19,
    n_lag = 0,
    mode = "slide",
    exceed = FALSE
  )

  expect_equal(sort(unique(split_frame$split)), 1:3)

  de_split_1 <- split_frame |>
    filter(bidding_zone == "DE", split == 1)

  de_split_3 <- split_frame |>
    filter(bidding_zone == "DE", split == 3)

  expect_equal(de_split_1$train[[1]], 1:60)
  expect_equal(de_split_1$test[[1]], 61:80)

  expect_equal(de_split_3$train[[1]], 41:100)
  expect_equal(de_split_3$test[[1]], 101:120)
})


test_that("make_split supports type last", {
  skip_if_not_installed("dplyr")

  library(dplyr)

  context <- list(
    series_id = "bidding_zone",
    value_id = "value",
    index_id = "time"
  )

  main_frame <- elec_price |>
    filter(bidding_zone %in% c("DE", "FR")) |>
    group_by(bidding_zone) |>
    slice_head(n = 120) |>
    ungroup()

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

  expect_equal(sort(unique(split_frame$split)), 1:2)

  de_split_1 <- split_frame |>
    filter(bidding_zone == "DE", split == 1)

  de_split_2 <- split_frame |>
    filter(bidding_zone == "DE", split == 2)

  expect_equal(de_split_1$train[[1]], 1:95)
  expect_equal(de_split_1$test[[1]], 96:107)

  expect_equal(de_split_2$train[[1]], 13:107)
  expect_equal(de_split_2$test[[1]], 108:119)
})


test_that("make_split supports type prob", {
  skip_if_not_installed("dplyr")

  library(dplyr)

  context <- list(
    series_id = "bidding_zone",
    value_id = "value",
    index_id = "time"
  )

  main_frame <- elec_price |>
    filter(bidding_zone %in% c("DE", "FR")) |>
    group_by(bidding_zone) |>
    slice_head(n = 120) |>
    ungroup()

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

  expect_equal(sort(unique(split_frame$split)), 1:3)

  de_split_1 <- split_frame |>
    filter(bidding_zone == "DE", split == 1)

  de_split_3 <- split_frame |>
    filter(bidding_zone == "DE", split == 3)

  expect_equal(de_split_1$train[[1]], 1:60)
  expect_equal(de_split_1$test[[1]], 61:80)

  expect_equal(de_split_3$train[[1]], 41:100)
  expect_equal(de_split_3$test[[1]], 101:120)
})


test_that("make_split includes lagged observations in test splits", {
  skip_if_not_installed("dplyr")

  library(dplyr)

  context <- list(
    series_id = "bidding_zone",
    value_id = "value",
    index_id = "time"
  )

  main_frame <- elec_price |>
    filter(bidding_zone %in% c("DE", "FR")) |>
    group_by(bidding_zone) |>
    slice_head(n = 120) |>
    ungroup()

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

  de_split_1 <- split_frame |>
    filter(bidding_zone == "DE", split == 1)

  de_split_2 <- split_frame |>
    filter(bidding_zone == "DE", split == 2)

  expect_equal(de_split_1$train[[1]], 1:48)
  expect_equal(de_split_1$test[[1]], 47:72)

  expect_equal(de_split_2$train[[1]], 25:72)
  expect_equal(de_split_2$test[[1]], 71:96)
})


test_that("make_split creates exceeding out-of-sample splits", {
  skip_if_not_installed("dplyr")

  library(dplyr)

  context <- list(
    series_id = "bidding_zone",
    value_id = "value",
    index_id = "time"
  )

  main_frame <- elec_price |>
    filter(bidding_zone %in% c("DE", "FR")) |>
    group_by(bidding_zone) |>
    slice_head(n = 120) |>
    ungroup()

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

  expect_equal(sort(unique(split_frame$split)), 1:2)

  de_split_1 <- split_frame |>
    filter(bidding_zone == "DE", split == 1)

  de_split_2 <- split_frame |>
    filter(bidding_zone == "DE", split == 2)

  expect_equal(de_split_1$train[[1]], 1:96)
  expect_equal(de_split_1$test[[1]], 97:120)

  expect_equal(de_split_2$train[[1]], 25:120)
  expect_equal(de_split_2$test[[1]], 121:144)
})


test_that("make_split works with the documented elec_price setup", {
  skip_if_not_installed("dplyr")

  library(dplyr)

  context <- list(
    series_id = "bidding_zone",
    value_id = "value",
    index_id = "time"
  )

  main_frame <- elec_price |>
    filter(bidding_zone %in% c("DE", "FR"))

  fixed_split <- make_split(
    main_frame = main_frame,
    context = context,
    type = "first",
    value = 2400,
    n_ahead = 24,
    n_skip = 23,
    n_lag = 0,
    mode = "slide",
    exceed = FALSE
  )

  expanding_split <- make_split(
    main_frame = main_frame,
    context = context,
    type = "first",
    value = 2400,
    n_ahead = 24,
    n_skip = 23,
    n_lag = 0,
    mode = "stretch",
    exceed = FALSE
  )

  expect_s3_class(fixed_split, "tbl_df")
  expect_s3_class(expanding_split, "tbl_df")

  expect_named(fixed_split, c("bidding_zone", "split", "train", "test"))
  expect_named(expanding_split, c("bidding_zone", "split", "train", "test"))

  expect_equal(sort(unique(fixed_split$bidding_zone)), c("DE", "FR"))
  expect_equal(sort(unique(expanding_split$bidding_zone)), c("DE", "FR"))

  fixed_de_1 <- fixed_split |>
    filter(bidding_zone == "DE", split == 1)

  expanding_de_1 <- expanding_split |>
    filter(bidding_zone == "DE", split == 1)

  expanding_de_2 <- expanding_split |>
    filter(bidding_zone == "DE", split == 2)

  expect_equal(fixed_de_1$train[[1]], 1:2400)
  expect_equal(fixed_de_1$test[[1]], 2401:2424)

  expect_equal(expanding_de_1$train[[1]], 1:2400)
  expect_equal(expanding_de_1$test[[1]], 2401:2424)

  expect_equal(expanding_de_2$train[[1]], 1:2424)
  expect_equal(expanding_de_2$test[[1]], 2425:2448)
})
