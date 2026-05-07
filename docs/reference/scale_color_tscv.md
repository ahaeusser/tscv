# Create a tscv color scale

Create a `ggplot2` color scale based on a predefined `tscv` palette.

## Usage

``` r
scale_color_tscv(palette = "main", discrete = TRUE, reverse = FALSE, ...)
```

## Arguments

- palette:

  Character value. Name of the palette.

- discrete:

  Logical value. If `TRUE`, create a discrete color scale. If `FALSE`,
  create a continuous color scale.

- reverse:

  Logical value. If `TRUE`, the palette is reversed.

- ...:

  Additional arguments passed to
  [`ggplot2::discrete_scale()`](https://ggplot2.tidyverse.org/reference/discrete_scale.html)
  or
  [`ggplot2::scale_color_gradientn()`](https://ggplot2.tidyverse.org/reference/scale_gradient.html).

## Value

A `ggplot2` scale object.

## Details

`scale_color_tscv()` creates either a discrete or continuous color scale
for the `color` aesthetic.

For discrete variables, the function uses
[`ggplot2::discrete_scale()`](https://ggplot2.tidyverse.org/reference/discrete_scale.html).
For continuous variables, it uses
[`ggplot2::scale_color_gradientn()`](https://ggplot2.tidyverse.org/reference/scale_gradient.html).

Available palettes are `"main"`, `"cool"`, `"hot"`, `"mixed"`, and
`"grey"`.

## See also

Other data visualization:
[`plot_bar()`](https://ahaeusser.github.io/tscv/reference/plot_bar.md),
[`plot_density()`](https://ahaeusser.github.io/tscv/reference/plot_density.md),
[`plot_histogram()`](https://ahaeusser.github.io/tscv/reference/plot_histogram.md),
[`plot_line()`](https://ahaeusser.github.io/tscv/reference/plot_line.md),
[`plot_point()`](https://ahaeusser.github.io/tscv/reference/plot_point.md),
[`plot_qq()`](https://ahaeusser.github.io/tscv/reference/plot_qq.md),
[`scale_fill_tscv()`](https://ahaeusser.github.io/tscv/reference/scale_fill_tscv.md),
[`theme_tscv()`](https://ahaeusser.github.io/tscv/reference/theme_tscv.md),
[`tscv_cols()`](https://ahaeusser.github.io/tscv/reference/tscv_cols.md),
[`tscv_pal()`](https://ahaeusser.github.io/tscv/reference/tscv_pal.md)

## Examples

``` r
library(dplyr)

data <- M4_monthly_data |>
  filter(series %in% c("M23100", "M14395"))

plot_line(
  data = data,
  x = index,
  y = value,
  color = series,
  title = "M4 Monthly Time Series",
  subtitle = "Selected monthly series",
  xlab = "Time",
  ylab = "Value"
) +
  scale_color_tscv(palette = "main")
```
