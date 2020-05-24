
#' @title Print the split table as \code{gt} object.
#'
#' @description This function prints the split table as \code{gt} object.
#'
#' @param data A tibble given by the function \code{summarise_split(...)}.
#' @param include Integer value. The number of top and bottom rows to include. If \code{NULL}, all rows are included.
#'
#' @return An object of class \code{gt}.
#' @export

print_split <- function(data,
                        include = 3) {

  # Prepare data
  if (!is.null(include)) {
    n_splits <- length(split_tbl$split)
    keep_rows <- c(1:include, (n_splits - include + 1):n_splits)

    split_tbl <- split_tbl %>%
      filter(split %in% keep_rows) %>%
      mutate(split = as.character(split)) %>%
      add_row(
        split = "...",
        time_train = "...",
        time_test = "...",
        index_train = "...",
        index_test = "...",
        .after = include)
  }

  # Create gt object and format
  tbl <- split_tbl %>%
    gt() %>%
    tab_spanner(
      label = "Date",
      columns = vars(
        time_train, time_test)) %>%
    tab_spanner(
      label = "Index",
      columns = vars(
        index_train, index_test)) %>%
    cols_label(
      split = "Split",
      time_train = "Training",
      time_test = "Testing",
      index_train = "Training",
      index_test = "Testing") %>%
    cols_align(
      align = "center",
      columns = vars(
        split,
        time_train,
        time_test,
        index_train,
        index_test))

  return(tbl)
}
