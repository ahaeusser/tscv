# Create a name for a folder or file

The function creates a name for a folder or file.

## Usage

``` r
file_name(
  primary,
  extension = NULL,
  add_time = TRUE,
  time_format = "%Y%m%d_%H%M%S"
)
```

## Arguments

- primary:

  Character value. Primary file name to be used.

- extension:

  Character value. File name extension to be used (e.g. `".csv"`,
  `".rda"`).

- add_time:

  Logical value. If `TRUE`, a time stamp is added in front of the file
  name.

- time_format:

  Character value. The time format of the time stamp.

## Value

Character value.
