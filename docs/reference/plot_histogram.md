# Plot data as a histogram

Create a histogram for one or more numeric variables.

## Usage

``` r
plot_histogram(
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
  line_color = "grey35",
  line_width = 0.5,
  fill_color = "grey35",
  fill_alpha = 1,
  theme_set = theme_tscv(),
  theme_config = list(),
  ...
)
```

## Arguments

- data:

  A `data.frame`, `tibble`, or `tsibble` in long format.

- x:

  Unquoted column in `data` containing numeric values.

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

  Optional unquoted column in `data` used to map histogram outline and
  fill color.

- fill:

  Optional unquoted column in `data` used to map histogram fill color.
  Currently not used directly; use `color` for grouped histograms.

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

- line_color:

  Character value defining the histogram bar outline color. Ignored when
  `color` is supplied.

- line_width:

  Numeric value defining the histogram bar outline width.

- fill_color:

  Character value defining the histogram bar fill color. Ignored when
  `color` is supplied.

- fill_alpha:

  Numeric value between `0` and `1` defining bar transparency.

- theme_set:

  A complete `ggplot2` theme.

- theme_config:

  A named `list` with additional arguments passed to
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).

- ...:

  Further arguments passed to
  [`ggplot2::geom_histogram()`](https://ggplot2.tidyverse.org/reference/geom_histogram.html).

## Value

An object of class `ggplot`.

## Details

`plot_histogram()` is a convenience wrapper around
[`ggplot2::geom_histogram()`](https://ggplot2.tidyverse.org/reference/geom_histogram.html).
It is useful for visualizing the distribution of values across one or
more time series, models, groups, or residual sets.

The arguments `x`, `facet_var`, `color`, and `fill` are passed as
unquoted column names.

If `color` is supplied, both bar outline color and fill color are mapped
to that variable. In this case, `line_color` and `fill_color` are
ignored. If `color` is not supplied, all histogram bars use `line_color`
and `fill_color`.

Missing values are removed before plotting.

Additional arguments can be passed to
[`ggplot2::geom_histogram()`](https://ggplot2.tidyverse.org/reference/geom_histogram.html)
through `...`, for example `bins`, `binwidth`, or `boundary`.

Additional theme settings can be supplied through `theme_config`. This
should be a named list of arguments passed to
[`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).

## See also

Other data visualization:
[`plot_bar()`](https://ahaeusser.github.io/tscv/reference/plot_bar.md),
[`plot_density()`](https://ahaeusser.github.io/tscv/reference/plot_density.md),
[`plot_line()`](https://ahaeusser.github.io/tscv/reference/plot_line.md),
[`plot_point()`](https://ahaeusser.github.io/tscv/reference/plot_point.md),
[`plot_qq()`](https://ahaeusser.github.io/tscv/reference/plot_qq.md)

## Examples

``` r
library(dplyr)

data <- M4_monthly_data |>
  filter(series %in% c("M23100", "M14395"))

plot_histogram(
  data = data,
  x = value,
  facet_var = series,
  title = "Distribution of M4 Monthly Values",
  subtitle = "Histograms by series",
  xlab = "Value",
  ylab = "Count",
  bins = 20
)


plot_histogram(
  data = data,
  x = value,
  color = series,
  title = "Distribution of M4 Monthly Values",
  subtitle = "Grouped histograms by series",
  xlab = "Value",
  ylab = "Count",
  bins = 20,
  position = "identity",
  fill_alpha = 0.4
)
```
