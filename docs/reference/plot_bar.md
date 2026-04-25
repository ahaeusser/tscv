# Plot data as bar chart

Plot one or more variables as bar chart.

## Usage

``` r
plot_bar(
  data,
  x,
  y,
  position = "dodge",
  facet_var = NULL,
  facet_scale = "free",
  facet_nrow = NULL,
  facet_ncol = NULL,
  color = NULL,
  flip = FALSE,
  reorder = FALSE,
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  caption = NULL,
  bar_size = 0.75,
  bar_color = "grey35",
  bar_alpha = 1,
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

- position:

  Character value. Possible values are `"stack} or \code{"dodge`.

- facet_var:

  Unquoted column within `.data` (facet).

- facet_scale:

  Character value defining axis scaling. Possible values are `"free"`,
  `"fixed"`, `"free_x"` and `"free_y"`.

- facet_nrow:

  Integer value. The number of rows.

- facet_ncol:

  Integer value. The number of columns.

- color:

  Unquoted column within `.data` (color).

- flip:

  Logical value. If `TRUE`, the plot is flipped from horizontal to
  vertical.

- reorder:

  Logical value. If `TRUE`, the values are reordered within each facet.

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

- bar_size:

  Numeric value defining the line width.

- bar_color:

  Character value defining the line color (ignored if `color` is
  present).

- bar_alpha:

  Numeric value defining the transparency of the line.

- theme_set:

  A complete ggplot2 theme.

- theme_config:

  A list with further arguments passed to
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).

- ...:

  Currently not in use.

## Value

p An object of class ggplot.
