# Custom ggplot2 theme for tscv

Create the default `ggplot2` theme used by the `tscv` package.

## Usage

``` r
theme_tscv(
  base_size = 11,
  base_family = "",
  base_line_size = base_size/22,
  base_rect_size = base_size/22
)
```

## Arguments

- base_size:

  Numeric value. Base font size.

- base_family:

  Character value. Base font family.

- base_line_size:

  Numeric value. Base line width for line elements.

- base_rect_size:

  Numeric value. Base line width for rectangle elements.

## Value

A complete `ggplot2` theme.

## Details

`theme_tscv()` returns a complete `ggplot2` theme with a clean layout,
subtle grid lines, bottom legend placement, and formatting for plot
titles, subtitles, captions, facets, and axes.

The theme is used as the default theme in the plotting helpers of the
`tscv` package, such as
[`plot_line()`](https://ahaeusser.github.io/tscv/reference/plot_line.md),
[`plot_point()`](https://ahaeusser.github.io/tscv/reference/plot_point.md),
[`plot_bar()`](https://ahaeusser.github.io/tscv/reference/plot_bar.md),
[`plot_histogram()`](https://ahaeusser.github.io/tscv/reference/plot_histogram.md),
[`plot_density()`](https://ahaeusser.github.io/tscv/reference/plot_density.md),
and
[`plot_qq()`](https://ahaeusser.github.io/tscv/reference/plot_qq.md).

Since the returned object is a regular `ggplot2` theme, it can also be
added directly to any `ggplot` object with `+ theme_tscv()`.

## See also

Other data visualization:
[`plot_bar()`](https://ahaeusser.github.io/tscv/reference/plot_bar.md),
[`plot_density()`](https://ahaeusser.github.io/tscv/reference/plot_density.md),
[`plot_histogram()`](https://ahaeusser.github.io/tscv/reference/plot_histogram.md),
[`plot_line()`](https://ahaeusser.github.io/tscv/reference/plot_line.md),
[`plot_point()`](https://ahaeusser.github.io/tscv/reference/plot_point.md),
[`plot_qq()`](https://ahaeusser.github.io/tscv/reference/plot_qq.md),
[`scale_color_tscv()`](https://ahaeusser.github.io/tscv/reference/scale_color_tscv.md),
[`scale_fill_tscv()`](https://ahaeusser.github.io/tscv/reference/scale_fill_tscv.md),
[`tscv_cols()`](https://ahaeusser.github.io/tscv/reference/tscv_cols.md),
[`tscv_pal()`](https://ahaeusser.github.io/tscv/reference/tscv_pal.md)

## Examples

``` r
library(dplyr)
library(ggplot2)

data <- M4_monthly_data |>
  filter(series == "M23100") |>
  mutate(index = as.Date(index))

# Plot with the default tscv theme
plot_line(
  data = data,
  x = index,
  y = value,
  title = "M4 Monthly Time Series",
  subtitle = "Series M23100",
  xlab = "Time",
  ylab = "Value",
  theme_set = theme_tscv()
)


# The same plot with the default ggplot2 grey theme
plot_line(
  data = data,
  x = index,
  y = value,
  title = "M4 Monthly Time Series",
  subtitle = "Series M23100",
  xlab = "Time",
  ylab = "Value",
  theme_set = theme_grey()
)


# theme_tscv() can also be added to regular ggplot objects
ggplot(data, aes(x = index, y = value)) +
  geom_line(color = "grey35") +
  labs(
    title = "M4 Monthly Time Series",
    subtitle = "Series M23100",
    x = "Time",
    y = "Value"
  ) +
  theme_tscv()
```
