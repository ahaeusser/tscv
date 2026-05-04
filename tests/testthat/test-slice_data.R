
test_that("slice_train returns training data for fixed-window splits", {
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

  train_frame <- slice_train(
    main_frame = main_frame,
    split_frame = split_frame,
    context = context
  )

  expect_s3_class(train_frame, "tbl_df")
  expect_true(all(c("bidding_zone", "time", "value", "split") %in% names(train_frame)))
  expect_equal(sort(unique(train_frame$bidding_zone)), c("DE", "FR"))
  expect_equal(sort(unique(train_frame$split)), 1:3)
  expect_equal(nrow(train_frame), 2 * 3 * 48)

  de_data <- main_frame |>
    filter(bidding_zone == "DE")

  de_split_1 <- train_frame |>
    filter(bidding_zone == "DE", split == 1)

  de_split_2 <- train_frame |>
    filter(bidding_zone == "DE", split == 2)

  expect_equal(de_split_1$time, de_data$time[1:48])
  expect_equal(de_split_1$value, de_data$value[1:48])

  expect_equal(de_split_2$time, de_data$time[25:72])
  expect_equal(de_split_2$value, de_data$value[25:72])
})


test_that("slice_test returns test data for fixed-window splits", {
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

  test_frame <- slice_test(
    main_frame = main_frame,
    split_frame = split_frame,
    context = context
  )

  expect_s3_class(test_frame, "tbl_df")
  expect_true(all(c("bidding_zone", "time", "value", "split") %in% names(test_frame)))
  expect_equal(sort(unique(test_frame$bidding_zone)), c("DE", "FR"))
  expect_equal(sort(unique(test_frame$split)), 1:3)
  expect_equal(nrow(test_frame), 2 * 3 * 24)

  de_data <- main_frame |>
    filter(bidding_zone == "DE")

  de_split_1 <- test_frame |>
    filter(bidding_zone == "DE", split == 1)

  de_split_2 <- test_frame |>
    filter(bidding_zone == "DE", split == 2)

  expect_equal(de_split_1$time, de_data$time[49:72])
  expect_equal(de_split_1$value, de_data$value[49:72])

  expect_equal(de_split_2$time, de_data$time[73:96])
  expect_equal(de_split_2$value, de_data$value[73:96])
})


test_that("slice_train returns training data for expanding-window splits", {
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

  train_frame <- slice_train(
    main_frame = main_frame,
    split_frame = split_frame,
    context = context
  )

  expect_s3_class(train_frame, "tbl_df")
  expect_true(all(c("bidding_zone", "time", "value", "split") %in% names(train_frame)))
  expect_equal(sort(unique(train_frame$bidding_zone)), c("DE", "FR"))
  expect_equal(sort(unique(train_frame$split)), 1:3)
  expect_equal(nrow(train_frame), 2 * (48 + 72 + 96))

  de_data <- main_frame |>
    filter(bidding_zone == "DE")

  de_split_1 <- train_frame |>
    filter(bidding_zone == "DE", split == 1)

  de_split_2 <- train_frame |>
    filter(bidding_zone == "DE", split == 2)

  expect_equal(de_split_1$time, de_data$time[1:48])
  expect_equal(de_split_1$value, de_data$value[1:48])

  expect_equal(de_split_2$time, de_data$time[1:72])
  expect_equal(de_split_2$value, de_data$value[1:72])
})


test_that("slice_test returns test data for expanding-window splits", {
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

  test_frame <- slice_test(
    main_frame = main_frame,
    split_frame = split_frame,
    context = context
  )

  expect_s3_class(test_frame, "tbl_df")
  expect_true(all(c("bidding_zone", "time", "value", "split") %in% names(test_frame)))
  expect_equal(sort(unique(test_frame$bidding_zone)), c("DE", "FR"))
  expect_equal(sort(unique(test_frame$split)), 1:3)
  expect_equal(nrow(test_frame), 2 * 3 * 24)

  de_data <- main_frame |>
    filter(bidding_zone == "DE")

  de_split_1 <- test_frame |>
    filter(bidding_zone == "DE", split == 1)

  de_split_2 <- test_frame |>
    filter(bidding_zone == "DE", split == 2)

  expect_equal(de_split_1$time, de_data$time[49:72])
  expect_equal(de_split_1$value, de_data$value[49:72])

  expect_equal(de_split_2$time, de_data$time[73:96])
  expect_equal(de_split_2$value, de_data$value[73:96])
})


test_that("slice_test includes lagged observations when n_lag is used", {
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

  test_frame <- slice_test(
    main_frame = main_frame,
    split_frame = split_frame,
    context = context
  )

  expect_s3_class(test_frame, "tbl_df")
  expect_equal(sort(unique(test_frame$bidding_zone)), c("DE", "FR"))
  expect_equal(sort(unique(test_frame$split)), 1:3)
  expect_equal(nrow(test_frame), 2 * 3 * 26)

  de_data <- main_frame |>
    filter(bidding_zone == "DE")

  de_split_1 <- test_frame |>
    filter(bidding_zone == "DE", split == 1)

  de_split_2 <- test_frame |>
    filter(bidding_zone == "DE", split == 2)

  expect_equal(de_split_1$time, de_data$time[47:72])
  expect_equal(de_split_1$value, de_data$value[47:72])

  expect_equal(de_split_2$time, de_data$time[71:96])
  expect_equal(de_split_2$value, de_data$value[71:96])
})


test_that("slice_train and slice_test work with type last", {
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

  train_frame <- slice_train(
    main_frame = main_frame,
    split_frame = split_frame,
    context = context
  )

  test_frame <- slice_test(
    main_frame = main_frame,
    split_frame = split_frame,
    context = context
  )

  de_data <- main_frame |>
    filter(bidding_zone == "DE")

  de_train_1 <- train_frame |>
    filter(bidding_zone == "DE", split == 1)

  de_test_1 <- test_frame |>
    filter(bidding_zone == "DE", split == 1)

  expect_equal(de_train_1$time, de_data$time[1:95])
  expect_equal(de_train_1$value, de_data$value[1:95])

  expect_equal(de_test_1$time, de_data$time[96:107])
  expect_equal(de_test_1$value, de_data$value[96:107])
})


test_that("slice_train and slice_test work with type prob", {
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

  train_frame <- slice_train(
    main_frame = main_frame,
    split_frame = split_frame,
    context = context
  )

  test_frame <- slice_test(
    main_frame = main_frame,
    split_frame = split_frame,
    context = context
  )

  de_data <- main_frame |>
    filter(bidding_zone == "DE")

  de_train_1 <- train_frame |>
    filter(bidding_zone == "DE", split == 1)

  de_test_1 <- test_frame |>
    filter(bidding_zone == "DE", split == 1)

  de_train_3 <- train_frame |>
    filter(bidding_zone == "DE", split == 3)

  de_test_3 <- test_frame |>
    filter(bidding_zone == "DE", split == 3)

  expect_equal(de_train_1$time, de_data$time[1:60])
  expect_equal(de_train_1$value, de_data$value[1:60])

  expect_equal(de_test_1$time, de_data$time[61:80])
  expect_equal(de_test_1$value, de_data$value[61:80])

  expect_equal(de_train_3$time, de_data$time[41:100])
  expect_equal(de_train_3$value, de_data$value[41:100])

  expect_equal(de_test_3$time, de_data$time[101:120])
  expect_equal(de_test_3$value, de_data$value[101:120])
})


test_that("slice_train and slice_test work with documented elec_price setup", {
  skip_if_not_installed("dplyr")

  library(dplyr)

  context <- list(
    series_id = "bidding_zone",
    value_id = "value",
    index_id = "time"
  )

  main_frame <- elec_price |>
    filter(bidding_zone %in% c("DE", "FR"))

  split_frame <- make_split(
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

  train_frame <- slice_train(
    main_frame = main_frame,
    split_frame = split_frame,
    context = context
  )

  test_frame <- slice_test(
    main_frame = main_frame,
    split_frame = split_frame,
    context = context
  )

  expect_s3_class(train_frame, "tbl_df")
  expect_s3_class(test_frame, "tbl_df")

  expect_true(all(c("bidding_zone", "time", "value", "split") %in% names(train_frame)))
  expect_true(all(c("bidding_zone", "time", "value", "split") %in% names(test_frame)))

  expect_equal(sort(unique(train_frame$bidding_zone)), c("DE", "FR"))
  expect_equal(sort(unique(test_frame$bidding_zone)), c("DE", "FR"))

  de_data <- main_frame |>
    filter(bidding_zone == "DE")

  de_train_1 <- train_frame |>
    filter(bidding_zone == "DE", split == 1)

  de_test_1 <- test_frame |>
    filter(bidding_zone == "DE", split == 1)

  de_train_2 <- train_frame |>
    filter(bidding_zone == "DE", split == 2)

  de_test_2 <- test_frame |>
    filter(bidding_zone == "DE", split == 2)

  expect_equal(de_train_1$time, de_data$time[1:2400])
  expect_equal(de_train_1$value, de_data$value[1:2400])

  expect_equal(de_test_1$time, de_data$time[2401:2424])
  expect_equal(de_test_1$value, de_data$value[2401:2424])

  expect_equal(de_train_2$time, de_data$time[1:2424])
  expect_equal(de_train_2$value, de_data$value[1:2424])

  expect_equal(de_test_2$time, de_data$time[2425:2448])
  expect_equal(de_test_2$value, de_data$value[2425:2448])
})
