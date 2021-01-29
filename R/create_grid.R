
#' @title Create a grid of unique combinations
#'
#' @description The function creates a grid of unique combinations for
#'   key variables and splits.
#'
#' @param data \code{train} or \code{test} data from \code{split_data()}.
#'
#' @return A tibble containing the unique combinations.
#' @export

create_grid <- function(data) {

  # Relevant columns in data
  keys <- key(data)

  # Filter relevant columns to unique combinations
  data %>%
    as_tibble() %>%
    select(!!!syms(keys)) %>%
    distinct()
}
