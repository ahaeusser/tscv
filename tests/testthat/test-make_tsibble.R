
test_that("make_tsibble converts M4_monthly_data to a tsibble", {
  library(dplyr)
  library(tsibble)

  context <- list(
    series_id = "series",
    value_id = "value",
    index_id = "index"
  )

  main_frame <- M4_monthly_data |>
    filter(series %in% c("M23100", "M14395"))

  result <- make_tsibble(
    main_frame = main_frame,
    context = context
  )

  expect_s3_class(result, "tbl_ts")
  expect_true(is_tsibble(result))

  expect_equal(index_var(result), "index")
  expect_equal(key_vars(result), "series")

  expect_true(all(c("index", "series", "category", "value") %in% names(result)))
  expect_equal(sort(unique(result$series)), c("M14395", "M23100"))
  expect_equal(nrow(result), nrow(main_frame))
})


test_that("make_tsibble preserves observations and values", {
  library(dplyr)
  library(tsibble)

  context <- list(
    series_id = "series",
    value_id = "value",
    index_id = "index"
  )

  main_frame <- M4_monthly_data |>
    filter(series == "M23100")

  result <- make_tsibble(
    main_frame = main_frame,
    context = context
  )

  expect_s3_class(result, "tbl_ts")
  expect_equal(as_tibble(result), main_frame)

  expect_equal(result$index, main_frame$index)
  expect_equal(result$series, main_frame$series)
  expect_equal(result$value, main_frame$value)
})


test_that("make_tsibble works with elec_price and time index", {
  library(dplyr)
  library(tsibble)

  context <- list(
    series_id = "bidding_zone",
    value_id = "value",
    index_id = "time"
  )

  main_frame <- elec_price |>
    filter(bidding_zone %in% c("DE", "FR")) |>
    group_by(bidding_zone) |>
    slice_head(n = 48) |>
    ungroup()

  result <- make_tsibble(
    main_frame = main_frame,
    context = context
  )

  expect_s3_class(result, "tbl_ts")
  expect_true(is_tsibble(result))

  expect_equal(index_var(result), "time")
  expect_equal(key_vars(result), "bidding_zone")

  expect_equal(sort(unique(result$bidding_zone)), c("DE", "FR"))
  expect_equal(nrow(result), nrow(main_frame))
  expect_equal(result$value, main_frame$value)
})
