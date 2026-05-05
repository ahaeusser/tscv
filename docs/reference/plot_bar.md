# Plot data as a bar chart

Create a bar chart for grouped numeric values.

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

  A `data.frame`, `tibble`, or `tsibble` in long format.

- x:

  Unquoted column in `data` used on the x-axis.

- y:

  Unquoted column in `data` containing numeric values shown on the
  y-axis.

- position:

  Character value defining the bar position. Common values are `"dodge"`
  and `"stack"`.

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

  Optional unquoted column in `data` used to map bar fill color.

- flip:

  Logical value. If `TRUE`, the plot is flipped using
  [`ggplot2::coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html).

- reorder:

  Logical value. If `TRUE`, add
  [`tidytext::scale_x_reordered()`](https://juliasilge.github.io/tidytext/reference/reorder_within.html)
  for reordered facet labels.

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

- bar_size:

  Numeric value defining the bar border line width.

- bar_color:

  Character value defining the bar fill color. Ignored when `color` is
  supplied.

- bar_alpha:

  Numeric value between `0` and `1` defining bar transparency.

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

`plot_bar()` is a convenience wrapper around
[`ggplot2::geom_bar()`](https://ggplot2.tidyverse.org/reference/geom_bar.html)
with `stat = "identity"`. It is intended for data that already contains
summarized values, for example accuracy metrics, counts, or grouped
summary statistics.

The arguments `x`, `y`, `facet_var`, and `color` are passed as unquoted
column names.

If `color` is supplied, bar fill colors are mapped to that variable and
`bar_color` is ignored. If `color` is not supplied, all bars are drawn
using `bar_color`.

The argument `position` controls how bars are displayed when `color` is
supplied. Common values are `"dodge"` and `"stack"`.

If `flip = TRUE`, the x-axis and y-axis are swapped using
[`ggplot2::coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html).

If `reorder = TRUE`,
[`tidytext::scale_x_reordered()`](https://juliasilge.github.io/tidytext/reference/reorder_within.html)
is added. This is useful when the x-axis has been reordered within
facets with
[`tidytext::reorder_within()`](https://juliasilge.github.io/tidytext/reference/reorder_within.html).

Additional theme settings can be supplied through `theme_config`. This
should be a named list of arguments passed to
[`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).

## See also

Other data visualization:
[`plot_density()`](https://ahaeusser.github.io/tscv/reference/plot_density.md),
[`plot_histogram()`](https://ahaeusser.github.io/tscv/reference/plot_histogram.md),
[`plot_line()`](https://ahaeusser.github.io/tscv/reference/plot_line.md),
[`plot_point()`](https://ahaeusser.github.io/tscv/reference/plot_point.md),
[`plot_qq()`](https://ahaeusser.github.io/tscv/reference/plot_qq.md)

## Examples

``` r
library(dplyr)

context <- list(
  series_id = "series",
  value_id = "value",
  index_id = "index"
)

data <- M4_monthly_data |>
  filter(series %in% c("M23100", "M14395"))

stats <- summarise_stats(
  .data = data,
  context = context
)

plot_bar(
  data = stats,
  x = series,
  y = mean,
  title = "Average Value by Series",
  xlab = "Series",
  ylab = "Mean"
)
#> Warning: Ignoring unknown parameters: `size`


acf_data <- estimate_acf(
  .data = data,
  context = context,
  lag_max = 12
)

plot_bar(
  data = acf_data,
  x = lag,
  y = value,
  facet_var = series,
  title = "Autocorrelation by Series",
  subtitle = "Sample autocorrelation up to lag 12",
  xlab = "Lag",
  ylab = "ACF"
)
#> Warning: Ignoring unknown parameters: `size`
```
