# Plot data as a line chart

Create a line chart for one or more time series or grouped numeric
variables.

## Usage

``` r
plot_line(
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
  line_size = 0.75,
  line_type = "solid",
  line_color = "grey35",
  line_alpha = 1,
  theme_set = theme_tscv(),
  theme_config = list(),
  ...
)
```

## Arguments

- data:

  A `data.frame`, `tibble`, or `tsibble` in long format.

- x:

  Unquoted column in `data` used on the x-axis.

- y:

  Unquoted column in `data` containing numeric values shown on the
  y-axis.

- facet_var:

  Optional unquoted column in `data` used for faceting.

- facet_scale:

  Character value defining facet axis scaling. Common values are
  `"free"`, `"fixed"`, `"free_x"`, and `"free_y"`.

- facet_nrow:

  Optional integer. Number of rows in the facet layout.

- facet_ncol:

  Optional integer. Number of columns in the facet layout.

- color:

  Optional unquoted column in `data` used to map line color.

- title:

  Character value. Plot title.

- subtitle:

  Character value. Plot subtitle.

- xlab:

  Character value. Label for the x-axis.

- ylab:

  Character value. Label for the y-axis.

- caption:

  Character value. Plot caption.

- line_size:

  Numeric value defining the line width.

- line_type:

  Character or numeric value defining the line type.

- line_color:

  Character value defining the line color. Ignored when `color` is
  supplied.

- line_alpha:

  Numeric value between `0` and `1` defining line transparency.

- theme_set:

  A complete `ggplot2` theme.

- theme_config:

  A named `list` with additional arguments passed to
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).

- ...:

  Currently not used.

## Value

An object of class `ggplot`.

## Details

`plot_line()` is a convenience wrapper around
[`ggplot2::geom_line()`](https://ggplot2.tidyverse.org/reference/geom_path.html)
for plotting data in long format. It supports optional grouping by
color, optional faceting, and the default `tscv` theme.

The arguments `x`, `y`, `facet_var`, and `color` are passed as unquoted
column names.

If `color` is supplied, line colors are mapped to that variable and
`line_color` is ignored. If `color` is not supplied, all lines are drawn
using `line_color`.

Additional theme settings can be supplied through `theme_config`. This
should be a named list of arguments passed to
[`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).

## See also

Other data visualization:
[`plot_bar()`](https://ahaeusser.github.io/tscv/reference/plot_bar.md),
[`plot_density()`](https://ahaeusser.github.io/tscv/reference/plot_density.md),
[`plot_histogram()`](https://ahaeusser.github.io/tscv/reference/plot_histogram.md),
[`plot_point()`](https://ahaeusser.github.io/tscv/reference/plot_point.md),
[`plot_qq()`](https://ahaeusser.github.io/tscv/reference/plot_qq.md)

## Examples

``` r
library(dplyr)

data <- M4_monthly_data |>
  filter(series %in% c("M23100", "M14395"))

plot_line(
  data = data,
  x = index,
  y = value,
  facet_var = series,
  title = "M4 Monthly Time Series",
  subtitle = "Selected monthly series from the M4 forecasting competition",
  xlab = "Time",
  ylab = "Value",
  caption = "Data: M4 Forecasting Competition"
)


plot_line(
  data = data,
  x = index,
  y = value,
  color = series,
  title = "M4 Monthly Time Series",
  xlab = "Time",
  ylab = "Value"
)
```
