# Quantile-Quantile plot

Quantile-Quantile plot of one or more time series.

## Usage

``` r
plot_qq(
  data,
  x,
  facet_var = NULL,
  facet_scale = "free",
  facet_nrow = NULL,
  facet_ncol = NULL,
  color = NULL,
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  caption = NULL,
  point_size = 2,
  point_shape = 16,
  point_color = "grey35",
  point_fill = "grey35",
  point_alpha = 0.25,
  line_width = 0.25,
  line_type = "solid",
  line_color = "grey35",
  line_alpha = 1,
  band_color = "grey35",
  band_alpha = 0.25,
  theme_set = theme_tscv(),
  theme_config = list(),
  ...
)
```

## Arguments

- data:

  A `data.frame`, `tibble` or `tsibble` in long format.

- x:

  Unquoted column within `.data` containing numeric values.

- facet_var:

  Unquoted column within `.data` (facet).

- facet_scale:

  Character value defining axis scaling (`facet_var = "free"` or
  `facet_var = "fixed"`).

- facet_nrow:

  Integer value. The number of rows.

- facet_ncol:

  Integer value. The number of columns.

- color:

  Unquoted column within `.data` (color).

- title:

  Title of the plot.

- subtitle:

  Subtitle of the plot.

- xlab:

  Label for the x-axis.

- ylab:

  Label for the y-axis.

- caption:

  Caption of the plot.

- point_size:

  Numeric value defining the point size.

- point_shape:

  Integer value defining the point shape.

- point_color:

  Character value defining the point color (ignored if `color` is
  present).

- point_fill:

  Character value defining the fill color (ignored if `color` is
  present).

- point_alpha:

  Numeric value defining the transparency of points.

- line_width:

  Numeric value defining the line width (45-degree line).

- line_type:

  Integer value defining the line type (45-degree line).

- line_color:

  Character value defining the line color (45-degree line).

- line_alpha:

  Numeric value defining the transparency of the line.

- band_color:

  Character value defining the fill color of the confidence bands.

- band_alpha:

  Numeric value defining the transparency of the confidence bands.

- theme_set:

  A complete ggplot2 theme.

- theme_config:

  A list with further arguments passed to
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).

- ...:

  Further arguments passed to
  [`qqplotr::stat_qq_point()`](https://rdrr.io/pkg/qqplotr/man/stat_qq_point.html),
  `qqplotr::stat_qq_line()`,
  [`qqplotr::stat_qq_band()`](https://rdrr.io/pkg/qqplotr/man/stat_qq_band.html).

## Value

p An object of class ggplot.
