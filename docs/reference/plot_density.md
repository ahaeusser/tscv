# Plot the density via Kernel Density Estimator

Plot the density of one or more time series via Kernel Density
Estimator.

## Usage

``` r
plot_density(
  data,
  x,
  facet_var = NULL,
  facet_scale = "free",
  facet_nrow = NULL,
  facet_ncol = NULL,
  color = NULL,
  fill = NULL,
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  caption = NULL,
  line_width = 0.1,
  line_type = "solid",
  line_color = "grey35",
  fill_color = "grey35",
  fill_alpha = 0.5,
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

- fill:

  Unquoted column within `.data` (fill color).

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

- line_width:

  Numeric value defining the line width of the kernel density estimator.

- line_type:

  Integer value defining the line type of the kernel density estimator.

- line_color:

  Character value defining the line color of the kernel density
  estimator.

- fill_color:

  Character value defining the fill color for the area under the kernel
  density estimator.

- fill_alpha:

  Numeric value defining the transparency of the area under the kernel
  density estimator.

- theme_set:

  A complete ggplot2 theme.

- theme_config:

  A list with further arguments passed to
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).

- ...:

  Further arguments passed to
  [`geom_density()`](https://ggplot2.tidyverse.org/reference/geom_density.html).

## Value

p An object of class ggplot.
