# Create a tscv color palette

Create a color interpolation function based on one of the predefined
`tscv` palettes.

## Usage

``` r
tscv_pal(palette = "main", reverse = FALSE, ...)
```

## Arguments

- palette:

  Character value. Name of the palette.

- reverse:

  Logical value. If `TRUE`, the palette is reversed.

- ...:

  Additional arguments passed to
  [`grDevices::colorRampPalette()`](https://rdrr.io/r/grDevices/colorRamp.html).

## Value

A palette function that takes an integer and returns hexadecimal color
codes.

## Details

`tscv_pal()` returns a palette function created with
[`grDevices::colorRampPalette()`](https://rdrr.io/r/grDevices/colorRamp.html).
The returned function can be used to generate any number of colors from
the selected palette.

Available palettes are:

- `"main"`: blue, green, yellow.

- `"cool"`: blue, green.

- `"hot"`: yellow, orange, red.

- `"mixed"`: blue, green, yellow, orange, red.

- `"grey"`: light grey, dark grey.

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
[`theme_tscv()`](https://ahaeusser.github.io/tscv/reference/theme_tscv.md),
[`tscv_cols()`](https://ahaeusser.github.io/tscv/reference/tscv_cols.md)

## Examples

``` r
# Create a palette function
pal <- tscv_pal("main")

# Generate five colors
pal(5)
#> [1] "#4682B4" "#96905A" "#E69F00" "#739E39" "#009E73"

# Reverse the palette
tscv_pal("hot", reverse = TRUE)(5)
#> [1] "#F0E442" "#E2A121" "#D55E00" "#DD7E00" "#E69F00"

# Use generated colors in base R
barplot(
  height = c(3, 5, 4),
  col = tscv_pal("main")(3)
)
```
