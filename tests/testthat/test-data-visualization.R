# tests/testthat/test-data_visualization.R

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))


make_plot_data <- function() {
  M4_monthly_data |>
    filter(series %in% c("M23100", "M14395"))
}


make_plot_context <- function() {
  list(
    series_id = "series",
    value_id = "value",
    index_id = "index"
  )
}


expect_ggplot <- function(x) {
  expect_s3_class(x, "ggplot")
}


test_that("plot_line returns a ggplot object", {
  data <- make_plot_data()

  p <- plot_line(
    data = data,
    x = index,
    y = value,
    title = "M4 Monthly Time Series",
    xlab = "Time",
    ylab = "Value"
  )

  expect_ggplot(p)
  expect_equal(p$labels$title, "M4 Monthly Time Series")
  expect_equal(p$labels$x, "Time")
  expect_equal(p$labels$y, "Value")
  expect_equal(class(p$layers[[1]]$geom)[1], "GeomLine")
})


test_that("plot_line supports color mapping", {
  data <- make_plot_data()

  p <- plot_line(
    data = data,
    x = index,
    y = value,
    color = series
  )

  expect_ggplot(p)
  expect_equal(class(p$layers[[1]]$geom)[1], "GeomLine")
  expect_true("colour" %in% names(p$layers[[1]]$mapping))
})


test_that("plot_line supports faceting", {
  data <- make_plot_data()

  p <- plot_line(
    data = data,
    x = index,
    y = value,
    facet_var = series
  )

  expect_ggplot(p)
  expect_s3_class(p$facet, "FacetWrap")
})


test_that("plot_bar returns a ggplot object", {
  context <- make_plot_context()
  data <- make_plot_data()

  stats <- summarise_stats(
    .data = data,
    context = context
  )

  p <- plot_bar(
    data = stats,
    x = series,
    y = mean,
    title = "Average Value by Series",
    xlab = "Series",
    ylab = "Mean"
  )

  expect_ggplot(p)
  expect_equal(p$labels$title, "Average Value by Series")
  expect_equal(p$labels$x, "Series")
  expect_equal(p$labels$y, "Mean")
  expect_equal(class(p$layers[[1]]$geom)[1], "GeomBar")
})


test_that("plot_bar supports color mapping", {
  context <- make_plot_context()
  data <- make_plot_data()

  stats <- summarise_stats(
    .data = data,
    context = context
  )

  p <- plot_bar(
    data = stats,
    x = series,
    y = mean,
    color = series
  )

  expect_ggplot(p)
  expect_equal(class(p$layers[[1]]$geom)[1], "GeomBar")
  expect_true("fill" %in% names(p$layers[[1]]$mapping))
})


test_that("plot_bar supports faceting with ACF output", {
  context <- make_plot_context()
  data <- make_plot_data()

  acf_data <- estimate_acf(
    .data = data,
    context = context,
    lag_max = 12
  )

  p <- plot_bar(
    data = acf_data,
    x = lag,
    y = value,
    facet_var = series
  )

  expect_ggplot(p)
  expect_equal(class(p$layers[[1]]$geom)[1], "GeomBar")
  expect_s3_class(p$facet, "FacetWrap")
})


test_that("plot_bar supports coord flip", {
  context <- make_plot_context()
  data <- make_plot_data()

  stats <- summarise_stats(
    .data = data,
    context = context
  )

  p <- plot_bar(
    data = stats,
    x = series,
    y = mean,
    flip = TRUE
  )

  expect_ggplot(p)
  expect_s3_class(p$coordinates, "CoordFlip")
})


test_that("plot_density returns a ggplot object", {
  data <- make_plot_data()

  p <- plot_density(
    data = data,
    x = value,
    title = "Distribution of Values",
    xlab = "Value",
    ylab = "Density"
  )

  expect_ggplot(p)
  expect_equal(p$labels$title, "Distribution of Values")
  expect_equal(p$labels$x, "Value")
  expect_equal(p$labels$y, "Density")
  expect_equal(class(p$layers[[1]]$geom)[1], "GeomDensity")
})


test_that("plot_density supports color mapping", {
  data <- make_plot_data()

  p <- plot_density(
    data = data,
    x = value,
    color = series
  )

  expect_ggplot(p)
  expect_equal(class(p$layers[[1]]$geom)[1], "GeomDensity")
  expect_true("colour" %in% names(p$layers[[1]]$mapping))
  expect_true("fill" %in% names(p$layers[[1]]$mapping))
})


test_that("plot_density supports faceting", {
  data <- make_plot_data()

  p <- plot_density(
    data = data,
    x = value,
    facet_var = series
  )

  expect_ggplot(p)
  expect_s3_class(p$facet, "FacetWrap")
})


test_that("plot_histogram returns a ggplot object", {
  data <- make_plot_data()

  p <- plot_histogram(
    data = data,
    x = value,
    bins = 20,
    title = "Distribution of Values",
    xlab = "Value",
    ylab = "Count"
  )

  expect_ggplot(p)
  expect_equal(p$labels$title, "Distribution of Values")
  expect_equal(p$labels$x, "Value")
  expect_equal(p$labels$y, "Count")
  expect_equal(class(p$layers[[1]]$geom)[1], "GeomBar")
  expect_equal(class(p$layers[[1]]$stat)[1], "StatBin")
})


test_that("plot_histogram supports color mapping", {
  data <- make_plot_data()

  p <- plot_histogram(
    data = data,
    x = value,
    color = series,
    bins = 20
  )

  expect_ggplot(p)
  expect_equal(class(p$layers[[1]]$geom)[1], "GeomBar")
  expect_equal(class(p$layers[[1]]$stat)[1], "StatBin")
  expect_true("colour" %in% names(p$layers[[1]]$mapping))
  expect_true("fill" %in% names(p$layers[[1]]$mapping))
})


test_that("plot_histogram supports faceting", {
  data <- make_plot_data()

  p <- plot_histogram(
    data = data,
    x = value,
    facet_var = series,
    bins = 20
  )

  expect_ggplot(p)
  expect_s3_class(p$facet, "FacetWrap")
})


test_that("plot_point returns a ggplot object", {
  data <- make_plot_data()

  p <- plot_point(
    data = data,
    x = index,
    y = value,
    title = "M4 Monthly Time Series",
    xlab = "Time",
    ylab = "Value"
  )

  expect_ggplot(p)
  expect_equal(p$labels$title, "M4 Monthly Time Series")
  expect_equal(p$labels$x, "Time")
  expect_equal(p$labels$y, "Value")
  expect_equal(class(p$layers[[1]]$geom)[1], "GeomPoint")
})


test_that("plot_point supports color mapping", {
  context <- make_plot_context()
  data <- make_plot_data()

  acf_data <- estimate_acf(
    .data = data,
    context = context,
    lag_max = 12
  )

  p <- plot_point(
    data = acf_data,
    x = lag,
    y = value,
    color = series
  )

  expect_ggplot(p)
  expect_equal(class(p$layers[[1]]$geom)[1], "GeomPoint")
  expect_true("colour" %in% names(p$layers[[1]]$mapping))
})


test_that("plot_point supports faceting", {
  data <- make_plot_data()

  p <- plot_point(
    data = data,
    x = index,
    y = value,
    facet_var = series
  )

  expect_ggplot(p)
  expect_s3_class(p$facet, "FacetWrap")
})


test_that("plot_qq returns a ggplot object", {
  skip_if_not_installed("qqplotr")

  data <- make_plot_data()

  p <- plot_qq(
    data = data,
    x = value,
    title = "QQ Plot of M4 Monthly Values"
  )

  expect_ggplot(p)
  expect_equal(p$labels$title, "QQ Plot of M4 Monthly Values")
  expect_equal(length(p$layers), 3)
})


test_that("plot_qq supports color mapping", {
  skip_if_not_installed("qqplotr")

  data <- make_plot_data()

  p <- plot_qq(
    data = data,
    x = value,
    color = series
  )

  expect_ggplot(p)
  expect_equal(length(p$layers), 3)
  expect_true("colour" %in% names(p$layers[[1]]$mapping))
})


test_that("plot_qq supports faceting", {
  skip_if_not_installed("qqplotr")

  data <- make_plot_data()

  p <- plot_qq(
    data = data,
    x = value,
    facet_var = series
  )

  expect_ggplot(p)
  expect_s3_class(p$facet, "FacetWrap")
  expect_equal(length(p$layers), 3)
})


test_that("theme_tscv returns a complete ggplot2 theme", {
  theme <- theme_tscv()

  expect_s3_class(theme, "theme")
  expect_true(isTRUE(attr(theme, "complete")))
  expect_equal(theme$legend.position, "bottom")
  expect_s3_class(theme$plot.title, "element_text")
  expect_s3_class(theme$panel.grid.major, "element_line")
})


test_that("theme_tscv respects base size", {
  theme <- theme_tscv(base_size = 14)

  expect_s3_class(theme, "theme")
  expect_equal(theme$text$size, 14)
})


test_that("tscv_cols returns all colors by default", {
  cols <- tscv_cols()

  expect_type(cols, "character")
  expect_true(
    all(
      c(
        "steelblue",
        "orange",
        "green",
        "purple",
        "red",
        "skyblue",
        "yellow",
        "dark grey",
        "light grey"
      ) %in% names(cols)
    )
  )
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", cols)))
})


test_that("tscv_cols returns selected colors", {
  cols <- tscv_cols("steelblue", "orange")

  expect_named(cols, c("steelblue", "orange"))
  expect_equal(length(cols), 2)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", cols)))
})


test_that("tscv_cols errors for unknown colors", {
  expect_error(
    tscv_cols("blue"),
    "Unknown tscv color name"
  )
})


test_that("tscv_pal returns a palette function", {
  pal <- tscv_pal("main")
  cols <- pal(5)

  expect_type(pal, "closure")
  expect_equal(length(cols), 5)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", cols)))
})


test_that("tscv_pal can reverse a palette", {
  pal <- tscv_pal("hot")
  pal_reverse <- tscv_pal("hot", reverse = TRUE)

  expect_equal(pal(3), rev(pal_reverse(3)))
})


test_that("tscv_pal errors for unknown palettes", {
  expect_error(
    tscv_pal("unknown"),
    "Unknown tscv palette"
  )
})


test_that("scale_color_tscv returns a discrete ggplot2 scale", {
  scale <- scale_color_tscv()

  expect_s3_class(scale, "ScaleDiscrete")
})


test_that("scale_color_tscv returns a continuous ggplot2 scale", {
  scale <- scale_color_tscv(discrete = FALSE)

  expect_s3_class(scale, "ScaleContinuous")
})


test_that("scale_fill_tscv returns a discrete ggplot2 scale", {
  scale <- scale_fill_tscv()

  expect_s3_class(scale, "ScaleDiscrete")
})


test_that("scale_fill_tscv returns a continuous ggplot2 scale", {
  scale <- scale_fill_tscv(discrete = FALSE)

  expect_s3_class(scale, "ScaleContinuous")
})


test_that("scale_color_tscv can be added to a line plot", {
  data <- make_plot_data()

  p <- plot_line(
    data = data,
    x = index,
    y = value,
    color = series
  ) +
    scale_color_tscv()

  expect_ggplot(p)
  expect_true(length(p$scales$scales) >= 1)
})


test_that("scale_fill_tscv can be added to a bar plot", {
  context <- make_plot_context()
  data <- make_plot_data()

  stats <- summarise_stats(
    .data = data,
    context = context
  )

  p <- plot_bar(
    data = stats,
    x = series,
    y = mean,
    color = series
  ) +
    scale_fill_tscv()

  expect_ggplot(p)
  expect_true(length(p$scales$scales) >= 1)
})
