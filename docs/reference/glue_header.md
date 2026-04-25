# Create a header from text

Create a header from text with `glue()`. White space is padded with "-"
to a line length of `n_char`.

## Usage

``` r
glue_header(text, n_char = 80, ft_bold = FALSE, ft_italic = FALSE)
```

## Arguments

- text:

  Character value. The text for the header.

- n_char:

  Integer value. Maximum length of the header line.

- ft_bold:

  Logical value. If `TRUE`, `text` is printed bold.

- ft_italic:

  Logical value. If `TRUE`, `text` is printed italic.

## Value

An object of class `glue`.
