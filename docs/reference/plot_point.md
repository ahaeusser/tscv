# Plot data as scatterplot

Plot one or more variables as scatterplot.

## Usage

``` r
plot_point(
  data,
  x,
  y,
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
  point_size = 1.5,
  point_type = 16,
  point_color = "grey35",
  point_alpha = 1,
  theme_set = theme_tscv(),
  theme_config = list(),
  ...
)
```

## Arguments

- data:

  A `data.frame`, `tibble` or `tsibble` in long format.

- x:

  Unquoted column within `.data`.

- y:

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

- point_type:

  Numeric value defining the point type.

- point_color:

  Character value defining the point color (ignored if `color` is
  present).

- point_alpha:

  Numeric value defining the transparency of the points.

- theme_set:

  A complete ggplot2 theme.

- theme_config:

  A list with further arguments passed to
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).

- ...:

  Currently not in use.

## Value

p An object of class ggplot.
